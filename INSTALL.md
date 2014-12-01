# Installing TeX2page

Get the tex2page distribution from github:

```
git clone https://github.com/ds26gte/tex2page
```

This produces a directory called `tex2page`, which
contains, among other things, the files

```
tex2page
tex2page.tex
tex2page.sty
```

Put copies of (or link to) the files `tex2page.tex` and
`tex2page.sty` in a directory that is in your
`TEXINPUTS` path.

## Configuring TeX2page for your machine

If your dialect is Racket, you may directly use the
`tex2page` script in this directory.  Put it in your
`PATH`.  For Windows, put the batch file `tex2page.bat`
in your `PATH`.  Correct  the pathnames of `racket` and
`tex2page` in `tex2page.bat` if needed.

If you use Common Lisp, you may copy the script
`tex2page.lisp` as
`tex2page` into your `PATH`.

For other Scheme dialects and also for Common Lisp (if you
wish to have a machine-translation of the Scheme tex2page),
the following method usually has a good chance of working on
Unix-like systems.  If you are not connected to the Internet
and don't have git, it will help to ensure that you have
Scmxlate already installed on your system.  Scmxlate is
available at https://github.com/ds26gte/scmxlate.

Type

```
./configure --help
```

to see the list of dialects the `configure` script supports.  If
your dialect `D` is one of them, type

```
./configure --dialect=D
```

This creates a file `my-tex2page`, which is a version
of `tex2page` configured for the dialect.

Note: If you are on a Unix-like environment blessed with
lots of GNU software, e.g. Linux and Cygwin, chances are you
already have or can easily install Guile.  On such systems,
simply type

```
./configure --dialect=guile
```

If your dialect is not supported, or the `configure` fails for
whatever reason, you can invoke Scmxlate directly.
First ensure that Scmxlate is installed on your system.

Start your Scheme or Common Lisp in the `tex2page`
directory; then explicitly load the file
`scmxlate.scm`, using its correct pathname.  E.g.,

```
(load "/usr/local/lib/scmxlate/scmxlate.scm")
```

You will be asked a few questions.  A choice of answers
will be provided, so you don't have to be too creative.
A file called `my-tex2page` is created.  On Windows, a
batch file called `tex2page.bat` is also created.

You can rename `my-tex2page` to `tex2page` and put it
in your `PATH`.  On Windows, move `tex2page.bat` to your
`PATH`, and also edit its contents so that the pathnames
it refers to are correct.

## Can't create a working script file?

If you are unable to create a script file to put in
your `PATH`, you may simply load `tex2page` (or
`my-tex2page`) directly into your Scheme, and
then call the Scheme procedure `tex2page` on your
source document, e.g.,

```
(load "my-tex2page")
(tex2page "jobname.tex")
```

## Image generation

(Skip this section if you're *not* running Windows.)

The image generation subprocess in tex2page uses
Ghostscript.  The tex2page code stores the name of the
Ghostscript executable name in the variable
`*ghostscript*`.  For Unix, this value is `"gs"`, which
is pretty standard.  If you're using Windows, check
that the value assigned to `*ghostscript*` is correct
for you.

## Generating the documentation

The tex2page documentation is provided in
the TeX file `index.tex`.  To get a PDF
version of the document run, first ensure
that the file `story.log` exists.  If it does
not, run

```
pdftex story
```

and supply

```
\end
```

at TeX's interactive prompt.  Then do

```
pdftex index
makeindex index
mpost lambda
pdftex index
pdftex index
```

The first TeX run may fail due to missing files.
Simply type `s` at the TeX prompt to force TeX
to continue.

The multiple runs of TeX are needed to resolve
cross-references.

If the MetaPost executable
is named `mp` on your system (e.g., MiKTeX),
change `mpost` to `mp`.

To get an HTML version of the document, run

```
tex2page index
```

a few times.  As with TeX, multiple runs of tex2page
are needed to resolve cross-references, but each
run of tex2page will explicitly tell you if another
run is necessary.

The WWW copy of the HTML documentation (with a
link for downloading the distribution) is at
http://www.ccs.neu.edu/~dorai/tex2page/index.html.