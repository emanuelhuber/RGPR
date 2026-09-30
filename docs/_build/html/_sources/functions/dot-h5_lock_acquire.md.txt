# Acquire an exclusive lock for a GPRsurvey backing file

```r
.h5_lock_acquire(dsn, timeout = 3, poll = 0.25)
```

## Arguments

- `dsn`: (`character(1)`) Path to the `.h5` file being protected. The lock does not need `dsn` to exist yet (it is also used while creating a brand-new file).
- `timeout`: (`numeric(1)`) Maximum number of seconds to wait for the lock before raising an error.
- `poll`: (`numeric(1)`) Seconds to sleep between lock attempts.

## Returns

(`character(1)`) The lock directory path. Pass this to `.h5_lock_release()` to release the lock.

## Description

Creates a lock directory `paste0(dsn, ".lock")`. Directory creation is an atomic operation on every platform R supports, so this is safe against race conditions between two processes without requiring extra packages. Waits (polling) until the lock is available or `timeout` is reached.


