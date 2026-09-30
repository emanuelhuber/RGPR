# ============================================================================ #
# HDF5 backing-file engine (internal)
# ============================================================================ #
#
# This file provides the low-level machinery shared by every function that
# writes to a GPRsurvey HDF5 backing file:
#
#   - locking          : `.h5_lock_acquire()` / `.h5_lock_release()`
#   - safe updates     : `.h5_update_survey()` (temp copy -> mutate -> verify
#                         -> atomic replace, all under a single open handle)
#   - typed writers    : `.h5_write_vector()`, `.h5_write_matrix()`,
#                         `.h5_write_data_array()`, `.h5_write_r_object()`
#   - checksum check   : `.h5_verify_checksums()`
#   - survey group I/O : `.write_survey_group_hdf5()` / `.read_survey_group_hdf5()`
#   - intersections I/O: `.write_intersections_hdf5()` / `.read_intersections_hdf5()`
#   - misc             : `.delete_h5_link_if_exists()`, `.copy_h5_line_group()`,
#                         `.normalizeMarkers()`
#
# DESIGN NOTES (why the file looks the way it does)
# -------------------------------------------------------------------------- #
# 1. Every write to an *existing* backing file goes through
#    `.h5_update_survey()`. It never opens the real file in write mode
#    directly. Instead it:
#       a) takes an exclusive lock on `dsn` (see below),
#       b) copies `dsn` to a temporary file *in the same directory*,
#       c) opens that temporary file ONCE and hands the open handle to a
#          caller-supplied function that performs all the mutations,
#       d) closes the file,
#       e) (optionally) re-opens it read-only and reads every dataset back,
#          which forces HDF5 to validate the fletcher32 checksums written
#          in step (c) -- this is our "backup integrity check",
#       f) atomically renames the temp file onto `dsn` with `file.rename()`
#          (same filesystem => atomic on POSIX and Windows).
#    If anything fails at any point, `dsn` is never touched: the temp file
#    is discarded and the previous backup is left exactly as it was. This
#    replaces the old pattern of opening/closing the real file 2-3 times per
#    logical update, which was neither atomic nor crash-safe.
#
# 2. Locking uses a lock *directory* (`paste0(dsn, ".lock")`). `dir.create()`
#    is atomic on every platform R supports (it maps to a single `mkdir()`
#    syscall), so this needs no extra dependency. If the `filelock` package
#    is installed, a real OS-level file lock could be substituted; the
#    directory-based approach here is the dependency-free default.
#
# 3. All datasets (not just the big radar-data array) are written with the
#    fletcher32 checksum filter, which HDF5 can only apply to *chunked*
#    datasets, so everything below is written chunked, even tiny metadata
#    vectors. Compression is applied only where it's likely to be worth the
#    CPU cost (the radar-data array and, if large, per-line coordinates);
#    small metadata arrays are chunked+checksummed but not compressed.
# ============================================================================ #


# ------------------------------------------------------------------------- #
# Locking
# ------------------------------------------------------------------------- #

#' Acquire an exclusive lock for a GPRsurvey backing file
#'
#' Creates a lock directory `paste0(dsn, ".lock")`. Directory creation is an
#' atomic operation on every platform R supports, so this is safe against
#' race conditions between two processes without requiring extra packages.
#' Waits (polling) until the lock is available or `timeout` is reached.
#'
#' @param dsn (`character(1)`) Path to the `.h5` file being protected. The
#'   lock does not need `dsn` to exist yet (it is also used while creating a
#'   brand-new file).
#' @param timeout (`numeric(1)`) Maximum number of seconds to wait for the
#'   lock before raising an error.
#' @param poll (`numeric(1)`) Seconds to sleep between lock attempts.
#'
#' @return (`character(1)`) The lock directory path. Pass this to
#'   `.h5_lock_release()` to release the lock.
#' @keywords internal
#' @noRd
.h5_lock_acquire <- function(dsn, timeout = 3, poll = 0.25) {
  lockdir <- paste0(dsn, ".lock")
  start   <- Sys.time()
  
  repeat {
    if (dir.create(lockdir, showWarnings = FALSE)) {
      info_con <- file(file.path(lockdir, "info"), open = "w")
      writeLines(
        c(paste("pid:", Sys.getpid()), paste("time:", format(Sys.time()))),
        info_con
      )
      close(info_con)
      return(lockdir)
    }
    
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout) {
      stop(
        "Could not acquire a lock on '", dsn, "' within ", timeout, " seconds.\n",
        "Another R session may currently be writing to this GPRsurvey file.\n",
        "If you are certain no other process is using it, remove the stale ",
        "lock directory manually:\n  ", lockdir,
        call. = FALSE
      )
    }
    
    Sys.sleep(poll)
  }
}

#' Release a lock acquired with `.h5_lock_acquire()`
#' @param lockdir (`character(1)`) Value returned by `.h5_lock_acquire()`.
#' @keywords internal
#' @noRd
.h5_lock_release <- function(lockdir) {
  if (!is.null(lockdir) && dir.exists(lockdir)) {
    unlink(lockdir, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}


# ------------------------------------------------------------------------- #
# Temporary file helpers
# ------------------------------------------------------------------------- #

#' Build a temporary path in the same directory as `dsn`
#'
#' Using the same directory guarantees `file.rename()` is an atomic,
#' same-filesystem operation when the file is later swapped into place.
#' @keywords internal
#' @noRd
.h5_temp_path <- function(dsn) {
  file.path(
    dirname(dsn),
    paste0(".", basename(dsn), ".tmp-", Sys.getpid(), "-",
           format(Sys.time(), "%Y%m%d%H%M%OS6"))
  )
}

#' Atomically replace `dsn` with `tmp`
#'
#' Tries `file.rename()` first (atomic, instantaneous, same filesystem).
#' Falls back to copy+remove only if the rename fails (e.g. `tmp` and `dsn`
#' end up on different filesystems/mounts for some reason) -- in that case
#' the operation is no longer atomic, so this is a best-effort fallback, not
#' the primary path.
#' @keywords internal
#' @noRd
.h5_atomic_replace <- function(tmp, dsn) {
  if (file.rename(tmp, dsn)) {
    return(invisible(dsn))
  }
  
  ok <- file.copy(tmp, dsn, overwrite = TRUE)
  if (!ok) {
    stop(
      "Failed to replace '", dsn, "' with the updated temporary file '", tmp, "'.\n",
      "The original file was left untouched; your update was NOT applied.",
      call. = FALSE
    )
  }
  unlink(tmp, force = TRUE)
  invisible(dsn)
}


# ------------------------------------------------------------------------- #
# Checksum verification
# ------------------------------------------------------------------------- #

#' Recursively read every dataset under an HDF5 group
#'
#' Reading a dataset that was written with the fletcher32 filter forces HDF5
#' to validate its checksum; a mismatch raises an error. This function's only
#' purpose is to trigger that validation for every dataset in the file.
#' @keywords internal
#' @noRd
.h5_walk_and_read <- function(grp, verbose = FALSE) {
  for (nm in names(grp)) {
    obj <- grp[[nm]]
    
    tryCatch(
      {
        if (inherits(obj, "H5Group")) {
          .h5_walk_and_read(obj, verbose = verbose)
        } else {
          if(isTRUE(verbose)) message("Checking: ", obj$get_obj_name())
          invisible(obj$read())
        }
      },
      finally = {
        try(obj$close(), silent = TRUE)
      }
    )
  }
  
  invisible(NULL)
}
# .h5_walk_and_read <- function(grp) {
#   for (nm in names(grp)) {
#     obj <- grp[[nm]]
#     if (inherits(obj, "H5Group")) {
#       .h5_walk_and_read(obj)
#     } else {
#       invisible(obj$read)
#     }
#     try(obj$close(), silent = TRUE)
#   }
#   invisible(NULL)
# }

#' Verify the checksums of every dataset in an HDF5 file
#'
#' Opens `path` read-only and reads every dataset in it, which forces HDF5
#' to validate the fletcher32 checksums that were set when the datasets were
#' written (see `.h5_write_vector()`, `.h5_write_matrix()`,
#' `.h5_write_data_array()`). Intended to be run on the *temporary* copy of a
#' backing file, right before it is atomically swapped into place, so that a
#' corrupted write is caught before it ever becomes "the backup".
#'
#' @param path (`character(1)`) Path to the HDF5 file to verify.
#' @return `TRUE`, invisibly, if every dataset reads back cleanly. Raises an
#'   error otherwise.
#' @keywords internal
#' @noRd
.h5_verify_checksums <- function(path, verbose = FALSE) {
  h5 <- hdf5r::H5File$new(path, mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  
  tryCatch(
    .h5_walk_and_read(h5, verbose = verbose),
    error = function(e) {
      stop(
        "Checksum verification failed for '", path, "': ", conditionMessage(e),
        "\nThe file appears to be corrupted; it was NOT used to replace the ",
        "previous backup.",
        call. = FALSE
      )
    }
  )
  invisible(TRUE)
}


# ------------------------------------------------------------------------- #
# Safe update wrapper: temp copy -> mutate once -> verify -> atomic replace
# ------------------------------------------------------------------------- #

#' Safely apply changes to an existing GPRsurvey HDF5 backing file
#'
#' This is the single entry point used by every function that *modifies* an
#' existing `.h5` backing file (as opposed to `GPRsurvey()`, which creates
#' one from scratch -- see that function for the equivalent "create" version
#' of this same pattern).
#'
#' Workflow:
#' 1. Acquire a lock on `dsn` (see `.h5_lock_acquire()`).
#' 2. Copy `dsn` to a temporary file in the same directory.
#' 3. Open that temporary file **once**, in `"a"` (read/write) mode.
#' 4. Call `FUN(h5)`, where `h5` is the open [hdf5r::H5File] handle. `FUN`
#'    should perform *all* the mutations for this update (e.g. write updated
#'    coordinates for several lines, then rewrite `/survey/intersections`)
#'    so the whole logical update happens under one open file handle.
#' 5. Flush and close the file.
#' 6. If `verify = TRUE` (default), read every dataset back to validate
#'    checksums (`.h5_verify_checksums()`).
#' 7. Atomically replace `dsn` with the temporary file.
#'
#' If any step fails, `dsn` is left completely untouched: the lock is
#' released, the temporary file is deleted, and the error propagates to the
#' caller.
#'
#' @param dsn (`character(1)`) Path to the existing `.h5` backing file.
#' @param FUN (`function(h5)`) Function called once with the open, writable
#'   [hdf5r::H5File] handle on the *temporary* copy. Its return value is
#'   passed back to the caller of `.h5_update_survey()`.
#' @param verify (`logical(1)`) Re-read every dataset after writing to
#'   validate checksums before the file is swapped in. Default `TRUE`;
#'   set to `FALSE` to skip the extra read pass on very large surveys where
#'   the write has already been verified by other means.
#' @param timeout (`numeric(1)`) Seconds to wait for the lock before giving up.
#'
#' @return Whatever `FUN` returned, invisibly.
#' @keywords internal
#' @noRd
.h5_update_survey <- function(dsn, FUN, verify = TRUE, timeout = 30) {
  
  dsn <- normalizePath(dsn, mustWork = TRUE)
  
  lock <- .h5_lock_acquire(dsn, timeout = timeout)
  tmp  <- .h5_temp_path(dsn)
  
  if (!file.copy(dsn, tmp, overwrite = TRUE)) {
    .h5_lock_release(lock)
    stop("Could not create a temporary working copy of '", dsn, "'.", call. = FALSE)
  }
  
  h5 <- hdf5r::H5File$new(tmp, mode = "a")
  
  # Registered in the exact order they must run at exit: close the file
  # handle first (required before the temp file can be deleted/renamed on
  # some platforms), then delete the temp file (a no-op once the atomic
  # replace below has already renamed it away), then release the lock last.
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  on.exit(.h5_lock_release(lock), add = TRUE)
  
  result <- FUN(h5)
  
  h5$flush()
  h5$close_all()
  
  if (isTRUE(verify)) {
    .h5_verify_checksums(tmp)
    message("integrity checked!")
  }
  
  .h5_atomic_replace(tmp, dsn)
  
  invisible(result)
}


#' Safely apply changes to a GPRsurvey HDF5 file that copies data in from a
#' *second*, different HDF5 file
#'
#' Same idea as `.h5_update_survey()`, extended for updates that need
#' read access to another backing file at the same time -- the canonical
#' example being `SU1[1:2] <- SU2[3:4]`, which copies line groups from
#' `SU2`'s file into `SU1`'s file. `SU1`'s file is updated via the usual
#' temp-copy + verify + atomic-replace sequence; `SU2`'s file is only ever
#' opened read-only.
#'
#' Both files are locked for the duration of the update, in a fixed order
#' (sorted by normalized path) regardless of which one is `dsn` and which
#' is `src_dsn`. This avoids a deadlock if two concurrent replacements run
#' in opposite directions at the same time (e.g. `SU1[i] <- SU2[j]` and
#' `SU2[k] <- SU1[l]` running in two different R sessions).
#'
#' @param dsn (`character(1)`) Path to the destination `.h5` backing file
#'   (the one being modified).
#' @param src_dsn (`character(1)`) Path to the source `.h5` backing file
#'   (read-only). May be identical to `dsn`, in which case this behaves
#'   like `.h5_update_survey()` with a single handle passed as both `h5`
#'   and `src_h5`.
#' @param FUN (`function(h5, src_h5)`) Called once with the open, writable
#'   handle on the temporary copy of `dsn` (`h5`) and the open, read-only
#'   handle on `src_dsn` (`src_h5`). Its return value is passed back to the
#'   caller.
#' @param verify,timeout See `.h5_update_survey()`.
#'
#' @return Whatever `FUN` returned, invisibly.
#' @keywords internal
#' @noRd
.h5_update_survey_with_source <- function(dsn, src_dsn, FUN, verify = TRUE, timeout = 30) {
  
  dsn     <- normalizePath(dsn, mustWork = TRUE)
  src_dsn <- normalizePath(src_dsn, mustWork = TRUE)
  
  if (identical(dsn, src_dsn)) {
    # Same file: there is only one handle to open. Reuse .h5_update_survey()
    # and hand FUN the same handle twice.
    return(.h5_update_survey(dsn, function(h5) FUN(h5, h5), verify = verify, timeout = timeout))
  }
  
  # ---- lock both files, always in the same (sorted) order ------------------ #
  paths      <- c(dsn, src_dsn)
  lock_order <- order(paths)
  locks <- vector("list", 2L)
  for (idx in lock_order) {
    locks[[idx]] <- .h5_lock_acquire(paths[idx], timeout = timeout)
  }
  
  tmp <- .h5_temp_path(dsn)
  if (!file.copy(dsn, tmp, overwrite = TRUE)) {
    for (idx in rev(lock_order)) .h5_lock_release(locks[[idx]])
    stop("Could not create a temporary working copy of '", dsn, "'.", call. = FALSE)
  }
  
  h5     <- hdf5r::H5File$new(tmp, mode = "a")
  src_h5 <- hdf5r::H5File$new(src_dsn, mode = "r")
  
  # Registered in the exact order they must run at exit -- mirrors
  # .h5_update_survey(): close both handles first (required before the temp
  # file can be deleted/renamed on some platforms), then delete the temp
  # file, then release both locks last.
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  on.exit(try(src_h5$close_all(), silent = TRUE), add = TRUE)
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  on.exit({
    for (idx in rev(lock_order)) .h5_lock_release(locks[[idx]])
  }, add = TRUE)
  
  result <- FUN(h5, src_h5)
  
  h5$flush()
  h5$close_all()
  src_h5$close_all()
  
  if (isTRUE(verify)) {
    .h5_verify_checksums(tmp)
  }
  
  .h5_atomic_replace(tmp, dsn)
  
  invisible(result)
}


# ------------------------------------------------------------------------- #
# Line group identity: opaque, position-based HDF5 group ids
# ------------------------------------------------------------------------- #
#
# Earlier versions of this code used each line's *display* name (e.g.
# `gpr@name`, "FILE____001", "2024/03/15", ...) directly as its HDF5 group
# name under `/lines`. That's fragile in two independent ways:
#
#   1. HDF5 treats `/` as a path separator even inside a single
#      `H5Group$create_group(name)` call, so a display name containing `/`
#      isn't created as one literal group -- HDF5 tries to traverse into a
#      nested path implied by the slash and fails with a traversal error.
#   2. Many readers give multiple, genuinely different input files the
#      *same* default display name (e.g. derived from a temp file created
#      during extraction, unrelated to the original input path), which
#      collides outright when used as a group name.
#
# `.h5_safe_name()` (below) patches (1), and the `safeName()` dedup in
# `GPRsurvey.R` patches (2) -- but both are still fixing symptoms of the
# same underlying design choice. The real fix is to stop giving the HDF5
# group name any meaning at all: `/lines` groups are named purely by
# **position** (`line000001`, `line000002`, ...), which is always a valid,
# unique HDF5 link name by construction, for any input whatsoever. The
# *display* name -- exactly what the user sees in `x@names`, with no
# restrictions on its characters or uniqueness beyond ordinary R vector
# naming -- lives only as data: the `name` attribute inside each group and
# the `/survey/names` dataset (see `.write_survey_group_hdf5()`).
#
# A side benefit: because a line's physical group id only depends on its
# *position* in the survey (never on its display name), replacing a line's
# contents in place (`[[<-`, `[<-`) no longer needs to create a
# differently-named group and delete the old one -- it just overwrites the
# group at that position. See `.replace_one_GPRsurvey_line_hdf5()` in
# `subset_GPRsurvey.R`.

#' Compute the opaque HDF5 group id for a line, from its position
#'
#' @param i (`integer`) 1-based position(s) in `x@names` (and equivalently
#'   in `/survey/names`, `/survey/nz`, etc. -- every per-line dataset is
#'   ordered the same way).
#' @return (`character`) e.g. `"line000001"`.
#' @keywords internal
#' @noRd
.h5_line_group_id <- function(i) {
  sprintf("line%06d", as.integer(i))
}

#' Resolve display names to HDF5 group ids via a survey's `/survey/names`
#'
#' Used when copying line groups between (or within) HDF5 files based on
#' another `GPRsurvey` object's `@names` (e.g. `SU1[1:2] <- SU2[3:4]`):
#' `value@names` are display names, but the *physical* groups that must be
#' copied are identified by position within `value@path`'s own
#' `/survey/names`, not by `value@names` directly.
#'
#' @param h5 Open [hdf5r::H5File] handle for the survey that `names_vec`
#'   belongs to.
#' @param names_vec (`character`) Display names to resolve (typically
#'   `value@names`, or a subset of it).
#' @return (`character`) HDF5 group ids, same length/order as `names_vec`.
#' @keywords internal
#' @noRd
.h5_resolve_line_ids <- function(h5, names_vec) {
  if (!"survey" %in% names(h5) || !"names" %in% names(h5[["survey"]])) {
    stop("Source HDF5 file has no '/survey/names' dataset.", call. = FALSE)
  }
  all_names <- h5[["survey"]][["names"]][]
  pos <- match(names_vec, all_names)
  if (anyNA(pos)) {
    stop(
      "Line(s) not found in the source survey's '/survey/names': ",
      paste(names_vec[is.na(pos)], collapse = ", "),
      call. = FALSE
    )
  }
  .h5_line_group_id(pos)
}



# ------------------------------------------------------------------------- #
# Safe HDF5 names (for things that still use a caller-supplied string
# directly as an HDF5 name, e.g. flattened `@md` metadata keys)
# ------------------------------------------------------------------------- #

#' Sanitize a string for use as an HDF5 group/dataset name
#'
#' HDF5 uses `/` as a path separator -- including inside a single
#' `H5Group$create_group(name)`/`create_dataset(name, ...)` call. A name
#' containing `/` is therefore *not* created as one literal group/dataset;
#' HDF5 tries to traverse into a nested path implied by the slash (e.g.
#' `"2024/03/15"` is read as "create `03/15` inside existing group
#' `2024`"), and fails with a traversal error if that parent path doesn't
#' already exist.
#'
#' Line groups themselves no longer need this (see the positional-id
#' scheme above), but a handful of places still turn an arbitrary,
#' caller-supplied string directly into an HDF5 name -- notably the
#' flattened, human-browsable copy of `GPR@md` keys in
#' `.write_GPR_line_hdf5()` (`writeHDF5.R`). This function protects those.
#'
#' @param name (`character(1)`) Proposed name.
#' @return (`character(1)`) `name` with `/`, `\\`, and control characters
#'   replaced by `_`. Falls back to `"default_name"` if `name` is empty,
#'   `NA`, or becomes empty after trimming.
#' @keywords internal
#' @noRd
.h5_safe_name <- function(name) {
  if (length(name) == 0L || is.na(name) || !nzchar(trimws(name))) {
    return("default_name")
  }
  name <- gsub("[/\\\\[:cntrl:]]", "_", name)
  name <- trimws(name)
  if (!nzchar(name)) name <- "default_name"
  name
}




# ------------------------------------------------------------------------- #
# Typed dataset writers (chunked + fletcher32 checksum on everything;
# gzip+shuffle compression only where it's worth the CPU cost)
# ------------------------------------------------------------------------- #


# ============================================================================ #
# Write / read nested R lists to / from HDF5 (hdf5r)
# ============================================================================ #
#
# Depends on helpers from hdf5_update.R:
#   .h5_safe_name(), .delete_h5_link_if_exists()
#
# DESIGN (same spirit as hdf5_update.R)
# -------------------------------------------------------------------------- #
# * list           -> HDF5 group. Child order, original (unsanitised) names and
#                     "unnamed list" status are stored as attributes, because
#                     HDF5 iterates links alphabetically, not in insertion order.
# * plain atomic   -> native HDF5 dataset (numeric / integer / logical /
#   (no attributes    character), chunked + fletcher32 checksum, optional
#   except `dim`)     shuffle+gzip for large arrays. Scalars, vectors, matrices
#                     and N-d arrays are all handled.
# * NULL           -> empty group tagged r_type = "NULL"  (round-trips)
# * anything else  -> lossless fallback: serialize() into a byte dataset
#   (factor, data.frame, named vector, complex, raw, character with NA,
#    zero-length vectors, S4 / S3 objects, ...)
# * names          -> sanitised with .h5_safe_name() ("/" would otherwise be
#                     read as an HDF5 path separator), de-duplicated, and the
#                     original name is restored on read.
# * existing link  -> replaced if overwrite = TRUE, error otherwise.
# * failure        -> the partially written node is removed again. For full
#                     atomicity wrap the call in .h5_update_survey().
#
# USAGE
# -------------------------------------------------------------------------- #
# vel <- list(vrms = 0.01, vint = c(0.1, 0.12, 0.11), v = matrix(runif(20), 5))
#
# h5 <- hdf5r::H5File$new("test.h5", mode = "a")
# write_list_h5(h5, "vel", vel)
# vel2 <- read_list_h5(h5, "vel")
# h5$close_all()
# identical(vel, vel2)
#
# # inside the safe-update machinery (temp copy -> write -> verify -> replace):
# .h5_update_survey(dsn, function(h5) {
#   write_list_h5(h5[["lines"]][[.h5_line_group_id(2)]], "vel", vel)
# })
# ============================================================================ #

#' Write an R object (typically a nested list) into an HDF5 group
#'
#' @param grp An open, writable `hdf5r` file or group.
#' @param name (`character(1)`) Name of the group/dataset to create under `grp`.
#' @param x The object to write. Lists become groups (recursively), plain
#'   numeric/integer/logical/character arrays become checksummed datasets,
#'   `NULL` becomes an empty tagged group, everything else is serialised.
#' @param overwrite (`logical(1)`) Replace an existing object called `name`.
#'   If `FALSE`, an existing object raises an error.
#' @param compress (`integer(1)`) gzip level 0-9 for numeric arrays with at
#'   least 1000 elements. Default `0` (no compression).
#' @return The HDF5 link name actually used (after sanitising), invisibly.
#' @noRd
.h5_write_list <- function(grp, name, x, overwrite = TRUE, compress = 0L) {
  
  if (length(name) != 1L || is.na(name) || !nzchar(trimws(name))) {
    stop("'name' must be a single, non-empty string.", call. = FALSE)
  }
  compress <- as.integer(compress)
  if (is.na(compress) || compress < 0L || compress > 9L) {
    stop("'compress' must be an integer between 0 and 9.", call. = FALSE)
  }
  
  id <- .h5_safe_name(name)
  
  if (grp$exists(id)) {
    if (!isTRUE(overwrite)) {
      stop("An HDF5 object named '", id, "' already exists in '",
           grp$get_obj_name(), "'. Use overwrite = TRUE to replace it.",
           call. = FALSE)
    }
    .delete_h5_link_if_exists(grp, id)
  }
  
  # From here on, a failure must not leave a half-written node behind.
  ok <- FALSE
  on.exit(if (!ok) try(.delete_h5_link_if_exists(grp, id), silent = TRUE),
          add = TRUE)
  
  .h5_write_node(grp, id, x, compress)
  ok <- TRUE
  invisible(id)
}

#' Read back an object written with `write_list_h5()`
#'
#' @param grp An open `hdf5r` file or group.
#' @param name (`character(1)`) Name of the group/dataset to read.
#' @return The reconstructed R object (list structure, names and order intact).
#' @noRd
.h5_read_list <- function(grp, name) {
  if (!grp$exists(name)) {
    stop("No HDF5 object named '", name, "' in '", grp$get_obj_name(), "'.",
         call. = FALSE)
  }
  obj <- grp[[name]]
  on.exit(try(obj$close(), silent = TRUE), add = TRUE)
  .h5_read_node(obj)
}

# Keep every chunk <= max_elems elements (~8 MB for doubles); HDF5 needs
# chunk dims >= 1 and <= dataset dims.
.h5_chunk_dims <- function(dims, max_elems = 2^20) {
  chunk <- as.numeric(dims)
  while (prod(chunk) > max_elems) {
    k <- which.max(chunk)
    chunk[k] <- ceiling(chunk[k] / 2)
  }
  as.integer(chunk)
}

# TRUE if x can be stored as a native HDF5 dataset without losing anything.
.h5_is_native <- function(x) {
  is.atomic(x) &&
    length(x) > 0L &&
    !is.object(x) &&
    (is.numeric(x) || is.logical(x) || is.character(x)) &&
    length(setdiff(names(attributes(x)), "dim")) == 0L &&
    !(is.character(x) && anyNA(x))
}

.h5_attr <- function(obj, key, default = NULL) {
  if (obj$attr_exists(key)) hdf5r::h5attr(obj, key) else default
}

# Chunked + checksummed dataset for numeric / integer / logical / character.
.h5_write_native <- function(grp, id, x, compress = 0L) {
  
  # hdf5r maps character vectors to variable-length strings; filters
  # are not applied to those (same rule as .h5_write_vector()).
  if (is.character(x)) {
    return(invisible(grp$create_dataset(name = id, robj = x)))
  }
  
  dims <- if (is.null(dim(x))) length(x) else dim(x)
  
  dcpl <- hdf5r::H5P_DATASET_CREATE$new()
  on.exit(try(dcpl$close(), silent = TRUE), add = TRUE)
  dcpl$set_chunk(.h5_chunk_dims(dims))
  if (compress > 0L && length(x) >= 1000L) {   # tiny arrays: not worth it
    dcpl$set_shuffle()
    dcpl$set_deflate(compress)
  }
  dcpl$set_fletcher32()
  
  invisible(grp$create_dataset(
    name              = id,
    robj              = x,
    dataset_create_pl = dcpl,
    chunk_dim         = NULL,
    gzip_level        = NULL
  ))
}

# Recursive worker: writes `x` as child `id` of `parent`.
.h5_write_node <- function(parent, id, x, compress) {
  
  # ---- NULL --------------------------------------------------------------- #
  if (is.null(x)) {
    g <- parent$create_group(id)
    on.exit(try(g$close(), silent = TRUE), add = TRUE)
    g$create_attr("r_type", "NULL")
    return(invisible(NULL))
  }
  
  # ---- plain list -> group ------------------------------------------------ #
  if (is.list(x) && !is.object(x)) {
    g <- parent$create_group(id)
    on.exit(try(g$close(), silent = TRUE), add = TRUE)
    
    n         <- length(x)
    nms       <- names(x)
    has_names <- !is.null(nms)
    if (!has_names) nms <- rep("", n)
    nms[is.na(nms)] <- ""
    
    ids <- vapply(seq_len(n), function(i) {
      if (nzchar(nms[i])) .h5_safe_name(nms[i]) else sprintf("item%06d", i)
    }, character(1))
    ids <- make.unique(ids, sep = "_")
    
    g$create_attr("r_type", "list")
    g$create_attr("has_names", as.integer(has_names))
    if (n > 0L) {
      g$create_attr("child_ids",   ids)   # physical link names, in order
      g$create_attr("child_names", nms)   # original names, in order
    }
    
    for (i in seq_len(n)) {
      .h5_write_node(g, ids[i], x[[i]], compress)
    }
    return(invisible(NULL))
  }
  
  # ---- native atomic -> dataset ------------------------------------------- #
  if (.h5_is_native(x)) {
    ds <- .h5_write_native(parent, id, x, compress)
    on.exit(try(ds$close(), silent = TRUE), add = TRUE)
    ds$create_attr("r_type", typeof(x))
    return(invisible(NULL))
  }
  
  # ---- fallback: lossless serialisation ----------------------------------- #
  ds <- .h5_write_native(parent, id, as.integer(serialize(x, connection = NULL)))
  on.exit(try(ds$close(), silent = TRUE), add = TRUE)
  ds$create_attr("r_type", "serialized")
  invisible(NULL)
}

# Recursive reader for one node (dataset or group).
.h5_read_node <- function(obj) {
  type <- .h5_attr(obj, "r_type", NULL)
  
  if (inherits(obj, "H5Group")) {
    if (identical(type, "NULL")) return(NULL)
    
    # Groups not written by write_list_h5() (no attributes): fall back to
    # alphabetical link order and the link names themselves.
    ids <- .h5_attr(obj, "child_ids", NULL)
    if (is.null(ids)) ids <- names(obj)
    
    out <- vector("list", length(ids))
    for (i in seq_along(ids)) {
      child <- obj[[ids[i]]]
      val   <- tryCatch(.h5_read_node(child), finally = try(child$close(), silent = TRUE))
      out[i] <- list(val)                    # keeps NULL elements in place
    }
    
    if (is.null(type)) {
      names(out) <- ids
    } else if (isTRUE(as.logical(.h5_attr(obj, "has_names", 0L)))) {
      names(out) <- .h5_attr(obj, "child_names", ids)
    }
    return(out)
  }
  
  if (identical(type, "serialized")) {
    return(unserialize(as.raw(obj$read())))
  }
  obj$read()
}


#' Write a 1-D vector as a chunked, checksummed HDF5 dataset
#'
#' Every dataset gets the fletcher32 checksum filter (HDF5 requires chunked
#' storage for this, which is why even tiny metadata vectors are chunked).
#' Compression is off by default (`compress = 0`) because it is rarely worth
#' the CPU cost for small metadata arrays; pass a gzip level for larger
#' vectors such as per-line coordinates if desired.
#'
#' @param grp An open, writable `hdf5r` group.
#' @param name (`character(1)`) Dataset name.
#' @param dta Atomic vector to write. If `length(data) == 0`, nothing is
#'   written (this keeps optional fields such as `z0` or `transf` absent
#'   from the file rather than present-but-empty).
#' @param compress (`integer(1)`) gzip level 0-9; `0` disables compression.
#' @keywords internal
#' @noRd
.h5_write_vector <- function(grp, name, dta, compress = 0L) {
  n <- length(dta)
  if (n == 0L) return(invisible(NULL))
  
  if (grp$link_exists(name)) {
    stop(
      "An HDF5 object already exists at '",
      grp$get_obj_name(), "/", name, "'.",
      call. = FALSE
    )
  }
  # hdf5r generally maps R character vectors to variable-length
  # HDF5 strings. Do not attach filters to these datasets.
  if (is.character(dta)) {
    ds <- grp$create_dataset(
      name = name,
      robj = dta
    )
    return(invisible(ds))
  }
  dcpl <- hdf5r::H5P_DATASET_CREATE$new()
  on.exit(
    try(dcpl$close(), silent = TRUE),
    add = TRUE
  )
  
  dcpl$set_chunk(n)
  if (compress > 0L) {
    dcpl$set_shuffle()   # byte-shuffle before deflate: usually improves the
    dcpl$set_deflate(compress)   # compression ratio a lot for numeric data
  }
  dcpl$set_fletcher32()
  
  ds <- grp$create_dataset(
    name              = name,
    robj              = dta,
    dataset_create_pl = dcpl,
    chunk_dim         = NULL,
    gzip_level        = NULL
  )
  invisible(ds)
}

#' Write a 2-D matrix as a chunked, checksummed HDF5 dataset
#' @inheritParams .h5_write_vector
#' @param data A matrix. If `NULL`, zero-length, or has zero rows, nothing
#'   is written.
#' @keywords internal
#' @noRd
.h5_write_matrix <- function(grp, name, data, compress = 0L) {
  if (is.null(data) || length(data) == 0L) return(invisible(NULL))
  if (!is.matrix(data)) data <- as.matrix(data)
  if (nrow(data) == 0L) return(invisible(NULL))
  
  dcpl <- hdf5r::H5P_DATASET_CREATE$new()
  dcpl$set_chunk(dim(data))
  if (compress > 0L) {
    dcpl$set_shuffle()
    dcpl$set_deflate(compress)
  }
  dcpl$set_fletcher32()
  
  ds <- grp$create_dataset(
    name              = name,
    robj              = data,
    dataset_create_pl = dcpl,
    chunk_dim         = NULL,
    gzip_level        = NULL
  )
  invisible(ds)
}

#' Write the main GPR data array (radargram) as 64-bit float, chunked and
#' checksummed, with optional gzip compression
#'
#' Unlike the earlier version of this code, the dataset type is always
#' `H5T_NATIVE_DOUBLE` (64-bit) -- the same precision R itself uses for
#' numeric vectors -- so writing to HDF5 never loses precision relative to
#' the in-memory `GPR` object.
#'
#' @param grp The (already created) HDF5 group for this line, i.e.
#'   `/lines/<name>`.
#' @param gpr Object of class `GPR`.
#' @param compress (`integer(1)`) gzip level 0-9; `0` disables compression.
#'   See the package documentation / `?GPRsurvey` for guidance on whether
#'   compression is worth it for GPR data.
#' @keywords internal
#' @noRd
.h5_write_data_array <- function(grp, gpr, compress = 0L) {
  nz <- nrow(gpr)
  nx <- ncol(gpr)
  
  # ---- guard against empty profiles --------------------------------------- #
  # A dataset with a zero-length dimension cannot be meaningfully chunked
  # (HDF5 requires every chunk dimension to be >= 1), and an "empty backup"
  # of a line is almost certainly a sign that something upstream went wrong
  # (e.g. a parsing error left the GPR object with no traces or no samples).
  # Failing loudly here is much easier to debug than discovering a silently
  # truncated/broken line later.
  if (nz == 0L || nx == 0L) {
    stop(
      "GPR line '", gpr@name[1L], "' is empty (nz = ", nz, ", nx = ", nx, "); ",
      "empty profiles cannot be written to HDF5.",
      call. = FALSE
    )
  }
  
  chunk_dims <- c(nz, min(nx, 128L))
  
  dcpl <- hdf5r::H5P_DATASET_CREATE$new()
  dcpl$set_chunk(chunk_dims)
  if (compress > 0L) {
    dcpl$set_shuffle()
    dcpl$set_deflate(compress)
  }
  dcpl$set_fletcher32()
  
  ds <- grp$create_dataset(
    name              = "data",
    dtype             = hdf5r::h5types$H5T_NATIVE_DOUBLE,
    space             = hdf5r::H5S$new(dims = c(nz, nx)),
    dataset_create_pl = dcpl,
    chunk_dim         = NULL,
    gzip_level        = NULL
  )
  ds[1:nz, 1:nx] <- gpr@data
  
  invisible(ds)
}


# ------------------------------------------------------------------------- #
# Lossless serialization of arbitrary R objects (used for `GPR@md`)
# ------------------------------------------------------------------------- #

#' Serialize an arbitrary R object into an HDF5 dataset
#'
#' Some slots -- notably `GPR@md`, the raw manufacturer metadata -- can
#' contain nested lists, `NULL`s, factors, or other structures that don't
#' map cleanly onto native HDF5 types. Rather than dropping everything that
#' isn't a length-one atomic scalar (as the previous version of this code
#' did), the *entire* object is serialized with [base::serialize()] into a
#' raw vector, stored as a 1-D array of unsigned bytes (0-255), and can be
#' perfectly reconstructed with `.h5_read_r_object()`.
#'
#' This is intentionally opaque to generic HDF5 tools (h5dump, HDFView,
#' h5py, ...) -- it is a backup mechanism, not a browsing format. See
#' `.write_GPR_line_hdf5()` for how this is paired with a second,
#' human-readable (but lossy) flattened copy for browsability.
#'
#' @param grp An open, writable `hdf5r` group.
#' @param name (`character(1)`) Dataset name.
#' @param obj Any R object understood by [base::serialize()].
#' @keywords internal
#' @noRd
.h5_write_r_object <- function(grp, name, obj) {
  bytes <- serialize(obj, connection = NULL)
  .delete_h5_link_if_exists(grp, name)
  .h5_write_vector(grp, name, as.integer(bytes), compress = 0L)
  invisible(NULL)
}

#' Read back an object written with `.h5_write_r_object()`
#' @param grp An open `hdf5r` group.
#' @param name (`character(1)`) Dataset name.
#' @return The original R object.
#' @keywords internal
#' @noRd
.h5_read_r_object <- function(grp, name) {
  vals <- grp[[name]][]
  unserialize(as.raw(vals))
}

# Read object if it exists, otherwise return default
.h5_read_if_exists <- function(grp, name, default = NULL) {
  if (grp$exists(name)) {
    grp[[name]]$read()
  } else {
    default
  }
}

# ------------------------------------------------------------------------- #
# Markers
# ------------------------------------------------------------------------- #

#' Normalize a markers vector to match the number of traces
#'
#' Ensures the markers vector for a line always has exactly `nx` elements
#' (one per trace), trimmed with [RGPR::trimStr()]. This guarantees the same,
#' consistent vector is used both for the per-line HDF5 dataset
#' (`/lines/<name>/markers`) and for the survey-level `@markers` slot, which
#' previously could disagree (`trimStr()` was applied in one place but not
#' the other).
#'
#' @param markers (`character`) Raw markers vector, typically `gpr@markers`.
#' @param nx (`integer(1)`) Number of traces the line has (`ncol(gpr)`).
#' @param verbose (`logical(1)`) Print a message when padding/truncating.
#' @return (`character[nx]`) Trimmed markers vector of length exactly `nx`.
#' @keywords internal
#' @noRd
.normalizeMarkers <- function(markers, nx, verbose = TRUE) {
  markers <- trimStr(markers)
  n <- length(markers)
  
  if (n == nx) return(markers)
  
  if (n == 0L) {
    return(rep("", nx))
  }
  
  if (n < nx) {
    verboseF(
      message("Markers vector shorter than the number of traces (",
              n, " < ", nx, "); padding with \"\"."),
      verbose = verbose
    )
    return(c(markers, rep("", nx - n)))
  }
  
  verboseF(
    message("Markers vector longer than the number of traces (",
            n, " > ", nx, "); truncating."),
    verbose = verbose
  )
  markers[seq_len(nx)]
}


# ------------------------------------------------------------------------- #
# Delete an HDF5 link if it exists
# ------------------------------------------------------------------------- #

#' @keywords internal
#' @noRd
.delete_h5_link_if_exists <- function(group, name) {
  if (length(name) != 1L || is.na(name) || !nzchar(name)) {
    return(invisible(FALSE))
  }
  if (!name %in% names(group)) {
    return(invisible(FALSE))
  }
  group$link_delete(name)
  invisible(TRUE)
}


# ------------------------------------------------------------------------- #
# Write coordinate matrix for selected GPRsurvey lines to HDF5
# ------------------------------------------------------------------------- #

#' Write updated trace coordinates for selected lines (internal)
#'
#' Unlike the previous version, this function takes an **already open**
#' `hdf5r` handle (supplied by `.h5_update_survey()`) instead of opening and
#' closing the backing file itself. Callers are responsible for wrapping
#' this in `.h5_update_survey()`.
#'
#' @param h5 Open, writable [hdf5r::H5File] handle.
#' @param obj Object of class `GPRsurvey` (already updated in memory).
#' @param ids (`integer`) Indices (into `obj@names`) of the lines whose
#'   coordinates changed.
#' @keywords internal
#' @noRd
.write_GPRsurvey_coords_hdf5 <- function(h5, obj, ids) {
  
  ids <- unique(as.integer(ids))
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0L) return(invisible(NULL))
  
  if (!"lines" %in% names(h5)) {
    stop("The HDF5 backing file has no '/lines' group.", call. = FALSE)
  }
  lines_group <- h5[["lines"]]
  
  for (id in ids) {
    line_name <- obj@names[id]
    if (!line_name %in% names(lines_group)) {
      stop(
        "Line group not found in HDF5 file: '/lines/", line_name, "'.",
        call. = FALSE
      )
    }
    
    coord <- obj@coords[[id]]
    if (is.null(coord) || length(coord) == 0L) next
    
    if (ncol(coord) != 3L) {
      stop(
        "Coordinates for line '", line_name, "' must have three columns.",
        call. = FALSE
      )
    }
    if (nrow(coord) != obj@nx[id]) {
      stop(
        "Number of coordinate rows for line '", line_name,
        "' does not match the number of traces.",
        call. = FALSE
      )
    }
    
    line_group  <- lines_group[[line_name]]
    coord_group <- if ("coords" %in% names(line_group)) {
      line_group[["coords"]]
    } else {
      line_group$create_group("coords")
    }
    
    .delete_h5_link_if_exists(coord_group, "xyz")
    .h5_write_matrix(coord_group, "xyz", coord)
  }
  
  invisible(NULL)
}


# Copy one line group between HDF5 files using hdf5r::copy_to()
#
# src_h5 and dst_h5 are open hdf5r::H5File objects.
# src_name and dst_name are group names under /lines.
#' @keywords internal
#' @noRd
.copy_h5_line_group <- function(src_h5, dst_h5, src_name, dst_name) {
  if (!"lines" %in% names(src_h5)) {
    stop("Source HDF5 file has no '/lines' group.", call. = FALSE)
  }
  if (!"lines" %in% names(dst_h5)) {
    dst_h5$create_group("lines")
  }
  
  src_lines <- src_h5[["lines"]]
  dst_lines <- dst_h5[["lines"]]
  
  if (!src_name %in% names(src_lines)) {
    stop(
      "Source line group not found in HDF5 file: '/lines/", src_name, "'.\n",
      "Available source line groups are:\n  ",
      paste(names(src_lines), collapse = ", "),
      call. = FALSE
    )
  }
  if (dst_name %in% names(dst_lines)) {
    dst_lines$link_delete(dst_name)
  }
  
  src_lines$obj_copy_to(
    dst_loc  = dst_lines,
    dst_name = dst_name,
    src_name = src_name
  )
  invisible(NULL)
}


# ------------------------------------------------------------------------- #
# Survey-level metadata group: write ALL slots, read ALL slots
# ------------------------------------------------------------------------- #

#' Write the complete `/survey` metadata group (internal)
#'
#' Writes every slot of a `GPRsurvey` object that makes sense to persist
#' into `/survey` of an open HDF5 file, replacing any previous `/survey`
#' group. This is the single source of truth for what a `GPRsurvey` HDF5
#' file contains at the survey level; `.read_survey_group_hdf5()` is its
#' exact mirror image on the read side.
#'
#' Slots intentionally **not** written here:
#' \itemize{
#'   \item `@view` -- this describes the *in-memory* object's relationship
#'         to its backing file (a lightweight subset view vs. an
#'         independent, writable survey), not a property of the file
#'         itself. A survey loaded directly from disk with
#'         `readGPRsurvey()` is always `view = FALSE`.
#'   \item `@coords`, `@markers` -- these already live per-line under
#'         `/lines/<name>/coords/xyz` and `/lines/<name>/markers`, which is
#'         the authoritative copy. Duplicating them at the survey level
#'         would risk the two copies drifting apart; `readGPRsurvey()`
#'         instead reconstructs `@coords`/`@markers` by reading the (small)
#'         per-line datasets directly.
#' }
#'
#' @param h5 Open, writable [hdf5r::H5File] handle.
#' @param obj Object of class `GPRsurvey`.
#' @keywords internal
#' @noRd
.write_survey_group_hdf5 <- function(h5, obj) {
  
  .delete_h5_link_if_exists(h5, "survey")
  sg <- h5$create_group("survey")
  
  # ---- scalar attributes --------------------------------------------------- #
  sg$create_attr("version", obj@version)
  sg$create_attr(
    "crs",
    if (length(obj@crs) == 0L || is.na(obj@crs[1L])) "" else obj@crs[1L]
  )
  sg$create_attr(
    "spunit",
    if (length(obj@spunit) == 0L || is.na(obj@spunit[1L])) "" else obj@spunit[1L]
  )
  
  # ---- per-line vectors (one element per line, in the same order as
  #      @names) -- every one of these is chunked + checksummed -------------  #
  .h5_write_vector(sg, "names",    obj@names)
  .h5_write_vector(sg, "paths",    obj@paths)
  .h5_write_vector(sg, "descs",    obj@descs)
  .h5_write_vector(sg, "modes",    obj@modes)
  .h5_write_vector(sg, "dates",    format(obj@dates, "%Y-%m-%d"))
  .h5_write_vector(sg, "freqs",    obj@freqs)
  .h5_write_vector(sg, "antseps",  obj@antseps)
  .h5_write_vector(sg, "nz",       obj@nz)
  .h5_write_vector(sg, "nx",       obj@nx)
  .h5_write_vector(sg, "zlengths", obj@zlengths)
  .h5_write_vector(sg, "xlengths", obj@xlengths)
  .h5_write_vector(sg, "zunits",   obj@zunits)
  
  # ---- optional survey-level fields ---------------------------------------- #
  if (length(obj@transf) > 0L) {
    .h5_write_vector(sg, "transf", obj@transf)
  }
  
  invisible(sg)
}

#' Read the complete `/survey` metadata group (internal)
#'
#' Mirror image of `.write_survey_group_hdf5()`. Handles files written by
#' older versions of this code that lack fields introduced later (`paths`,
#' `transf`) by falling back to sensible defaults.
#'
#' @param h5 Open, read-only (or read/write) [hdf5r::H5File] handle.
#' @return A named list with one element per `GPRsurvey` slot that
#'   `.write_survey_group_hdf5()` writes.
#' @keywords internal
#' @noRd
.read_survey_group_hdf5 <- function(h5) {
  
  if (!h5$exists("survey")) {
    stop(
      "This HDF5 file does not appear to be an RGPR GPRsurvey file ",
      "(missing '/survey' group).",
      call. = FALSE
    )
  }
  sg <- h5[["survey"]]
  
  .attr <- function(obj, key, default = "") {
    tryCatch(obj$attr_open(key)$read(), error = function(e) default)
  }
  .ds <- function(name, default) {
    if (sg$exists(name)) sg[[name]][] else default
  }
  
  names_ <- .ds("names", character(0))
  n      <- length(names_)
  
  crs_str <- .attr(sg, "crs", "")
  
  list(
    version  = .attr(sg, "version", "0.3"),
    crs      = if (nzchar(crs_str)) crs_str else NA_character_,
    spunit   = .attr(sg, "spunit", ""),
    names    = names_,
    paths    = .ds("paths",   character(n)),
    descs    = .ds("descs",   character(n)),
    modes    = .ds("modes",   character(n)),
    dates    = as.Date(.ds("dates", rep(NA_character_, n))),
    freqs    = .ds("freqs",   numeric(n)),
    antseps  = .ds("antseps", numeric(n)),
    nz       = as.integer(.ds("nz", integer(n))),
    nx       = as.integer(.ds("nx", integer(n))),
    zlengths = .ds("zlengths", numeric(n)),
    xlengths = .ds("xlengths", numeric(n)),
    zunits   = .ds("zunits",   character(n)),
    transf   = .ds("transf",   numeric(0))
  )
}


# ------------------------------------------------------------------------- #
# Intersections
# ------------------------------------------------------------------------- #

#' Write line intersection data into the HDF5 file
#'
#' Writes the `@intersections` slot (if non-empty) to `/survey/intersections`
#' as one dataset per named element. Assumes `/survey` already exists (i.e.
#' this is called after `.write_survey_group_hdf5()`).
#'
#' @param h5 Open, writable [hdf5r::H5File] handle.
#' @param obj `GPRsurvey` object, typically just returned by
#'   [RGPR::findIntersection()].
#' @keywords internal
#' @noRd
.write_intersections_hdf5 <- function(h5, obj) {
  if (!.hasSlot(obj, "intersections")) return(invisible(NULL))
  ints <- obj@intersections
  if (length(ints) == 0L) return(invisible(NULL))
  
  sg <- if ("survey" %in% names(h5)) h5[["survey"]] else h5$create_group("survey")
  
  .delete_h5_link_if_exists(sg, "intersections")
  ig <- sg$create_group("intersections")
  
  for (nm in names(ints)) {
    val <- ints[[nm]]
    if (is.numeric(val) && length(val) > 0L) {
      .h5_write_vector(ig, nm, val)
    }
  }
  
  invisible(NULL)
}

#' Read line intersection data back from the HDF5 file
#'
#' Mirror image of `.write_intersections_hdf5()`.
#'
#' @param h5 Open [hdf5r::H5File] handle.
#' @return A named list of numeric vectors (possibly empty).
#' @keywords internal
#' @noRd
.read_intersections_hdf5 <- function(h5) {
  if (!"survey" %in% names(h5)) return(list())
  sg <- h5[["survey"]]
  if (!sg$exists("intersections")) return(list())
  
  ig  <- sg[["intersections"]]
  out <- list()
  for (nm in names(ig)) {
    out[[nm]] <- ig[[nm]][]
  }
  out
}


# -------------------------------------------------------------------------
# ACCESS LINE GROUP ATTRIBUTE / VECTOR
# -------------------------------------------------------------------------

# # USAGE
# xcoord <- .h5_line_read(survey, 2, "x")
# xyz <- .h5_line_read(survey, 2, "coords/xyz")
# data <- .h5_line_read(survey, 2, "data")
# vel <- .h5_line_read(survey, 2, "vel/v")

.h5_line_read <- function(x, i, path) {
  .with_h5_line(x, i, function(grp) {
    parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
    obj <- grp
    for (p in parts) {
      if (!obj$exists(p)) {
        stop(
          "Path '/lines/", x@names[i], "/", path,
          "' does not exist.",
          call. = FALSE
        )
      }
      obj <- obj[[p]]
    }
    obj$read()
  })
}


.with_h5_line <- function(x, i, FUN) {
  stopifnot(inherits(x, "GPRsurvey"))
  if (length(i) != 1L || is.na(i)) {
    stop("'i' must be a single integer.", call. = FALSE)
  }
  i <- as.integer(i)
  if (i < 1L || i > length(x@names)) {
    stop(
      "'i' out of range [1,", length(x@names), "].",
      call. = FALSE
    )
  }
  if (!file.exists(x@path)) {
    stop(
      "Backing HDF5 file not found: '", x@path, "'.",
      call. = FALSE
    )
  }
  h5 <- hdf5r::H5File$new(x@path, mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  nm <- x@names[i]
  if (!h5[["lines"]]$exists(nm)) {
    stop(
      "Line group '/lines/", nm, "' does not exist in HDF5 file.",
      call. = FALSE
    )
  }
  grp <- h5[["lines"]][[nm]]
  FUN(grp)
}

# --------------------------------------------------------------------------

.h5_line_read_all <- function(x, path) {
  .with_h5_survey(x, function(h5) {
    lg <- h5[["lines"]]
    out <- vector("list", length(x@names))
    names(out) <- x@names
    parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
    for (i in seq_along(x@names)) {
      obj <- lg[[x@names[i]]]
      for (p in parts) {
        if (!obj$exists(p)) {
          stop(
            "Path '/lines/", x@names[i], "/", path,
            "' does not exist.",
            call. = FALSE
          )
        }
        obj <- obj[[p]]
      }
      out[[i]] <- obj$read()
    }
    out
  })
}

.with_h5_survey <- function(x, FUN) {
  stopifnot(inherits(x, "GPRsurvey"))
  if (!file.exists(x@path)) {
    stop(
      "Backing HDF5 file not found: '", x@path, "'.",
      call. = FALSE
    )
  }
  h5 <- hdf5r::H5File$new(x@path, mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
  FUN(h5)
}

# # ------- READ A VECTOR
# .h5_line_vector <- function(x, i, name) {
#   .with_h5_line(x, i, function(grp) {
#     if (!grp$exists(name)) {
#       stop(
#         "Dataset '", name,
#         "' not found in '/lines/", x@names[i], "'.",
#         call. = FALSE
#       )
#     }
#     grp[[name]]$read()
#   })
# }
# # ------- READ AN ATTRIBUTE
# .h5_line_attr <- function(x, i, name) {
#   .with_h5_line(x, i, function(grp) {
#     attrs <- names(grp$attr_open())
#     if (!name %in% attrs) {
#       stop(
#         "Attribute '", name,
#         "' not found in '/lines/", x@names[i], "'.",
#         call. = FALSE
#       )
#     }
#     hdf5r::h5attr(grp, name)
#   })
# }

