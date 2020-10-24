This specific repo is an attempt to document the Unicon compiler. This is
currently independent of the Unicon Project and is initially for my benefit in
undertsanding the various aspects of the Unicon compiler.

As I proceed with the documentation, I will be making various changes to the
codebase of the compiler itself. This will include various extractiosn of specific
procedures into their own files as well as some restructure of the code based on
various considerations that I have.

As I have certain interests tht are not necessarily aligned with the Unicon
Project, I am keeping this specific work as a separate project. At some future
time mayhaps, some of this code can be submitted and incorporated into the main
project. I will leave that to the discretion of the Unicon Project Team.

On of the future directions that is planned for this project is to provide a common
base for ucode/icode production directly from the Unicon compiler without having
to use the icont program. I would like to see if there is a way to provide a REPL
that can be incorporated.

To ensure that this work does not deviate from the current ucode production, any
changes made will be tested against the current Unicon compiler and icont program.

If the ucode files do not match, one must automatically consider that I have messed
up and this is to be considered an error on the part of this codebase.

If you want to test this codebase, do so at your own risk.
