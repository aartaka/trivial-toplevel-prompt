<h1>Trivial Toplevel Prompt</h1>

<p>
Trivial Toplevel Prompt is a petty portability library to set the
implementation-specific REPL prompt to a new, possibly more useful,
value. It allows showing the inspect/debug/stepping statuses alongside
the command number, current package, and current process or thread.

<p>
Tested on SBCL, CCL, ECL, ABCL, CLISP, Allegro CL (basically all the
implementations I can run), and CMUCL (untested, incomplete). Help in
supporting other implementations
(like <a href="http://www.lispworks.com/documentation/lw80/lw/lw-lispworks-87.htm#lispworks_marker-line-3817">LispWorks with its *prompt*</a>)
is much appreciated!

<h2 id=getting-started><a href=#getting-started>"Getting Started"</a></h2>

<p>
Clone the git repository:

<figure><pre><code>git clone https://github.com/aartaka/trivial-toplevel-prompt ~/common-lisp/
</code></pre><figcaption>Git command cloning trivial-toplevel-prompt</figcaption></figure>

<p>
And then load <code>:trivial-toplevel-prompt</code> in the REPL:

<figure><pre><code>(asdf:load-system :trivial-toplevel-prompt)
;; or, if you use Quicklisp
(ql:quickload :trivial-toplevel-prompt)
</code></pre><figcaption>Loading TTP via ASDF/Quicklisp</figcaption></figure>

<p>
You can also install Trivial Toplevel Prompt via Guix,
using the bundled <code>guix.scm</code> file:

<figure><pre><code>guix package -f guix.scm
</code></pre><figcaption>Guix command to install TTP</figcaption></figure>

<h2 id=apis><a href=#apis>APIs</a></h2>

<p>
The main API function is <code>set-toplevel-prompt</code>.
The values that it accepts are:

<DL><dt> String </dt> <dd>
use it as a format control.
</dd><dt> Function </dt> <dd>
use it to print the prompt to the REPL stream.
</DL>

<p>
Arguments provided to both the format control and the printing
function are (mostly modeled after Allegro CL APIs):

<UL><li> Stream to print the prompt to. Only provided for the function, implied for the string.
 </li><li> The current process/thread name (a string) or NIL when not provided/meaningless.
 </li><li> The shortest possible name/nickname of the current package
 (<code>cl:*package*</code>).
 </li><li> A number indicating the command/expression number.
 </li><li> A number indicating the current break/debug level.
 </li><li> A boolean indicating whether stepping is enabled.
 </li><li> A boolean indicating whether <code>inspect</code> is in progress.
</UL>

<p>
Note that some implementations have custom REPLs for stepping and
inspection,
so <code>set-toplevel-prompt</code> will be useless for inspect and stepping arguments.

<p>
Then there's CD(reset-toplevel-prompt) to undo the effect of CD(set-toplevel-prompt), basically restoring the previous prompt state.

<h2 id=examples><a href=#examples>Examples</a></h2>

<p>
Here's a simple format control example:
just skip the process name, print package name and command number.

<figure><pre><code>(trivial-toplevel-prompt:set-toplevel-prompt "~*~a~@[(~d)~]: ")
;; CL-USER(7):
</code></pre><figcaption>Simple prompt</figcaption></figure>

<p>
And a more involved, Allegro-style (<code>[4si] CL-USER(29):</code>) prompt:

<figure><pre><code>(trivial-toplevel-prompt:set-toplevel-prompt
 (lambda (stream process/thread-name package-name
          command-number debug-level stepping-p inspect-p)
   (declare (ignorable process/thread-name))
   (when (or debug-level stepping-p inspect-p)
     (format stream "[~@[~d~]~@[s~*~]~@[i~*~]] "
             debug-level stepping-p inspect-p))
   (format stream "~a~@[(~d)~]: " package-name command-number)))
</code></pre><figcaption>Allegro-style prompt</figcaption></figure>

<p>
And then reset it:

<figure><pre><code>(trivial-toplevel-prompt::reset-toplevel-prompt)
;; CL-USER(7):
(trivial-toplevel-prompt::reset-toplevel-prompt)
;; Back to implementation-specific prompt.
</code></pre><figcaption>Prompt reset</figcaption></figure>
