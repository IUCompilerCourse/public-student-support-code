# How to submit your code

Here is an outline of the steps you should make sure you have done when
submitting an assignment:

1. Make sure your git repo is in good shape. By "good shape", we mean:

  * _In the top-level directory_, you should have a file named `compiler.rkt`
    that provides the list of passes for that assignment. For example, if you
    are implementing a compiler for the R1 language, you should
    `(provide r1-passes)`, where `r1-passes` is the list of passes.

    Note that it is important for `compiler.rkt` to be in the top-level
    directory, as our testing infrastructure assumes this invariant. Don't
    put `compiler.rkt` in, say, a subdirectory in your repo, because our
    tests won't find it!

    In addition, make sure all the other files you need to run `compiler.rkt`
    are included in your repo. For example, if your `compiler.rkt` has
    `(require "utilities.rkt")`, make sure `utilities.rkt` is included in
    your repo as well. We test your code by simply `git clone`-ing your repo,
    so if you don't have `utilities.rkt` in the repo, we can't run your
    compiler!

  * Create a `README` file that lists the members of your team.

  * If you're implementing an assignment which requires typechecking, also
    provide a function named `typechecker`. Note that typechecking does NOT
    need to be one of your compiler passes! Our testing frameworks' functions
    (`interp-tests` and `compiler-tests`) both accept the `typecheck` function
    in place of `#f` as the second argument, so you can simply pass `typecheck`
    to these functions to have your programs automatically typechecked and
    fed to your list of passes.

  * If you are a undergraduate student who has implemented a challenge
    assignment for extra credit, state that you have done so in the
    `README` file. This helps the graders determine if we should look
    for this or not.

    You are also encouraged to put any other important info in a `README` file
    if you feel it is important for the graders to know (e.g., if you didn't
    complete all of the passes).

  * Your git repo should be accessible to the professor and the AIs.

2. Submit a tarball (`*.zip` or `*.tar.gz`) of your assignment to Canvas.
   Each Assignment should have a submission link on Canvas (if this isn't the
   case, please notify an AI!) where you can select the tarball to upload. The
   Assignment page also has a field for comments—in this field, please enter
   the URL of your git repo.

   Although our primary means of looking at your code is via the git repo you
   have given us access to, technology does occasionally fail. If your git repo
   is inaccessible for some reason, the tarball submission is our backup, so
   please make sure you do in fact submit one.
