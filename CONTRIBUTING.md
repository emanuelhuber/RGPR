
# How to contribute

## Bug reports

1. **Make sure your are working with the latest version of `RGPR`.**
2. **Create a short and simple reproducible example.**
    You can use the GPR data from `RGPR`: `frenkeLine00`, e.g.,
    ```r
    x <- dcshift(frenkeLine00, u = 1:25)
    ```
3. **Open a new issue on github.**
    Make sure that the title is clear as well as informative, the
    problem is well described, and don't forget to include a reproducible
    example.

## Share your wishes, user experience, etc.

Open a new issue on github or send me an email: emanuel.huber@alumni.ethz.ch

## Make changes

You are welcome to improve the code and add new functionalities. 
We encourage you to inform us about what you plan to add, maybe we are currently
working on that...

Please, try to follow our:

* [coding guide](https://github.com/emanuelhuber/RGPR/wiki/Coding-guide)
* [documenting guide](https://github.com/emanuelhuber/RGPR/wiki/How-to-document-with-roxygen2)

Follow the 
[fork workflow](https://github.com/emanuelhuber/RGPR/wiki/Fork-Workflow) to 
adapt the code locally and submit your changes (with pull requests). Make sure 
that you are working with the latest version of `RGPR` and use meaningfull
commits in present tense.

Once you submitted your code with a pull request, we will review your request.
Maybe we will aks you to modify some parts of your code before we can approve
your request. This is an iterative process.
