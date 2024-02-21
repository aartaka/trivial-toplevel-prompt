#include "preprocessor.h"
#define PRE()<pre>
#define PRECAP() </pre>
#define H2(ID, ...) <h2>__VA_ARGS__</h2>
<h1>Trivial Toplevel Prompt</h1>

<p>
Trivial Toplevel Prompt (TTP) is a petty portability library to set the
implementation-specific REPL prompt to a new, possibly more useful,
value. It allows showing the inspect/debug/stepping statuses alongside
the command number, current package, and current process or thread.

<p>
Tested on SBCL, CCL, ECL, ABCL, CLISP, Allegro CL (basically all the
implementations I can run), and CMUCL (untested, incomplete). Help in
supporting other implementations
(like A("http://www.lispworks.com/documentation/lw80/lw/lw-lispworks-87.htm#lispworks_marker-line-3817", LispWorks with its *prompt*))
is much appreciated!

SECTION2(getting-started, Getting Started)

<p>
Clone the git repository:

PRE()git clone https://github.com/aartaka/trivial-toplevel-prompt ~/common-lisp/
PRECAP()

<p>
And then load CD(:trivial-toplevel-prompt) in the REPL:

PRE()(asdf:load-system :trivial-toplevel-prompt)
;; or, if you use Quicklisp
(ql:quickload :trivial-toplevel-prompt)
PRECAP()

<p>
You can also install Trivial Toplevel Prompt via Guix,
using the bundled CD(guix.scm) file:

PRE()guix package -f guix.scm
PRECAP()

SECTION2(apis, APIs)

<p>
The main API function is CD(set-toplevel-prompt).
The values that it accepts are:

DL(String)
use it as a format control.
DD(Function)
use it to print the prompt to the REPL stream.
END(DL)

<p>
Arguments provided to both the format control and the printing
function are (mostly modeled after Allegro CL APIs):

ULI Stream to print the prompt to. Only provided for the function, implied for the string.
 LI The current process/thread name (a string) or NIL when not provided/meaningless.
 LI The shortest possible name/nickname of the current package
 (CD(cl:*package*)).
 LI A number indicating the command/expression number.
 LI A number indicating the current break/debug level.
 LI A boolean indicating whether stepping is enabled.
 LI A boolean indicating whether CD(inspect) is in progress.
END(UL)

<p>
Note that some implementations have custom REPLs for stepping and
inspection,
so CD(set-toplevel-prompt) will be useless for inspect and stepping arguments.

<p>
Then there's CD(reset-toplevel-prompt) to undo the effect of CD(set-toplevel-prompt), basically restoring the previous prompt state.

SECTION2(examples, Examples)

<p>
Here's a simple format control example:
just skip the process name, print package name and command number.

PRE()(trivial-toplevel-prompt:set-toplevel-prompt "~*~a~@[(~d)~]: ")
;; CL-USER(7):
PRECAP()

<p>
And a more involved, Allegro-style (CD([4si] CL-USER(29):)) prompt:

PRE()(trivial-toplevel-prompt:set-toplevel-prompt
 (lambda (stream process/thread-name package-name
          command-number debug-level stepping-p inspect-p)
   (declare (ignorable process/thread-name))
   (when (or debug-level stepping-p inspect-p)
     (format stream "[~@[~d~]~@[s~*~]~@[i~*~]] "
             debug-level stepping-p inspect-p))
   (format stream "~a~@[(~d)~]: " package-name command-number)))
;; [4si] CL-USER(29):
PRECAP()

<p>
And then reset it:

PRE()(trivial-toplevel-prompt::reset-toplevel-prompt)
;; CL-USER(7):
(trivial-toplevel-prompt::reset-toplevel-prompt)
;; Back to implementation-specific prompt.
PRECAP()
     