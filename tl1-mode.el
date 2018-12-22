;;; tl1-mode.el --- Fluke TL/1 mode                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Version: 1.0
;; URL: https://github.com/ieure/tl1-mode
;; Package-Requires: ((emacs "25"))
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is a major mode for editing Fluke TL/1 source code.

;;; Code:

(require 'eldoc)
(require 'cl-lib)
(require 'seq)

(defun tl1-mode--word-opt (words)
  "Return a regexp to match whole words in a string in the list WORDS."
  (concat "\\b" (regexp-opt words) "\\b"))

(defconst tl1-mode--keywords
  '("if"
    "then"
    "else"
    "else if"
    "end if"
    "global"
    "loop until"
    "loop while"
    "loop for"
    "loop"
    "end loop"
    "for"
    "next"
    "program"
    "end program"
    "function"
    "end function"
    "declare"
    "end declare"
    "global"
    "abort"
    "execute"
    "return"
    "goto"
    "addr"
    "data"
    "mask")
  "TL/1 built-in keywords.")

(defconst tl1-mode--keyword-re
  (tl1-mode--word-opt tl1-mode--keywords)

  "Regular expression for matching TL/1 built-in keywords.")

(defconst tl1-mode--function-documentation
  '(("abort" .
     ":abort:
  abort     !aborts execution or does a multi-level return
  abort x   !aborts execution or does a multi-level return (x returned)")

    ("acos" .
     ":acos:
  f = acos (0.245)  !f is set to the angle whose cosine is 0.245")

    ("arm" .
     ":arm:
  arm device \"/mod1,/probe\"  !arm module 1 and probe to gather responses
    device:  \"/probe\" (default), <I/O module name> or <clip module name>
             combinations of the above")

    ("ascii" .
     ":ascii:
  n = ascii (\"A\")  !n is set to the decimal number 65 (hex value 41)")

    ("asin" .
     ":asin:
  f = asin (0.455)  !f is set to the angle whose sine is 0.455")

    ("assign" .
     ":assign:
  s = assign device \"/mod3\"  !control module 3 independent of clip
                             !connections")

    ("assignment" .
     ":assignment:
  <variable> = <expression>
  n = $AA        !\"n = \" implies \"n\" is set to a numeric value
  s = \"ABC\"      !\"s = \" implies \"s\" is set to a string
  f = 1.2345E+1  !\"f = \" implies \"f\" is set to a floating point value")

    ("assoc" .
     ":assoc:
  assoc ref \"U5\", pins 40, device \"/mod1\" !set up for measurement on a
                                          !40-pin IC that is pre-connected
                                          !to U5 with I/O Module 1.
  ref:     <reference designator>
  pins:    <number of pins on the component>
  device:  <I/O module name> or <clip module name>
           combinations of the above")

    ("atan" .
     ":atan:
  f = atan (1.22)  !f is set to the angle whose tangent is 1.22")

    ("bitmask" .
     ":bitmask:
  n = bitmask 3  !n is set to F (bits 0 through 3 are set)")

    ("cflt" .
     ":cflt:
  f = cflt (109)  !converts the decimal numeric 109 to floating-point")

    ("checkstatus" .
     ":checkstatus:
  n = checkstatus device \"/mod3\"  !n is set to module 3 status
    device:  \"/probe\" (default), <I/O module name> or <clip module name>
             combinations of the above")

    ("chr" .
     ":chr:
  s = chr ($2B)  !s is set to the character \"+\"")

    ("clearoutputs" .
     ":clearoutputs:
  clearoutputs device \"/mod1,/mod2\" !3-state all outputs on modules 1 and 2
    device:  <I/O module name>, <clip module name> or <reference designator>
             (default = \"/mod1\")")

    ("clearpatt" .
     ":clearpatt:
  clearpatt device \"/mod1,/mod2\"  !clear stored pattern for modules 1 and 2
    device:  <I/O module name>, <clip module name> or <reference designator>
             (default = \"/mod1\")")

    ("clearpersvars" .
     ":clearpersvars:
  clearpersvars()       ! set all persistent variables to zero values")

    ("clip" .
     ":clip:
  s = clip ref \"U1\", pins 10  !tell operator to clip onto \"U1\", a ten
                              !pin IC,  s is set to the clip used
  ref:   <reference designator>
  pins:  <number of pins>")

    ("clockfreq" .
     ":clockfreq:
  clockfreq device \"/mod3\", freq \"1MHz\"
    device:  <I/O module name> or <clip module name> (default = \"/mod1\")
             combinations of the above
    freq:    \"1MHz\" (default), \"5MHz\", \"10MHz\" or \"20MHz\"")

    ("close" .
     ":close:
  close channel n  !close an open channel named n
                   !n evaluates to a valid channel number
    channel:  <numeric expression that evaluates to a valid channel number>")

    ("cnum" .
     ":cnum:
  n = cnum (100.6)  !n is set to 101 (the nearest numeric value)")

    ("compare" .
     ":compare:
  compare device \"/mod1\", patt \"10XX1101\", state \"enable\"
                    !enable compare DCE on module 1
    device:  <I/O module name> or <clip module name> (default = \"/mod1\")
    patt:    <string expression for comparison> (default = \"1\")
    state:   \"enable\"  (default) or \"disable\"")

    ("connect" .
     ":connect:
  connect device \"/mod1\", start \"U1-2\", stop \"AM-1\", clock \"U2-1\",
    enable \"U29-9\", common \"tp0\",  clear \"no\"
      !tell the operator to connect clocking inputs for module 1
    device:  \"/probe\" (default), <I/O module name> or <clip module name>
             combinations of the above
    start:   <ref pin for start>  (default = \"not used\")
    stop:    <ref pin for stop>   (default = \"not used\")
    clock:   <ref pin for clock>  (default = \"not used\")
    enable:  <ref pin for enable> (default = \"not used\")
    common:  <ref pin for common> (default = \"not used\")
    clear:   \"no\" (default) or \"yes\"")

    ("cos" .
     ":cos:
  f = cos (theta)  !f is set to the cosine of the variable theta")

    ("count" .
     ":count:
  n = count device \"/mod1\", pin 5, refpin \"u1-1\"
  !n is set to count of \"u1\" pin 1 (overriding \"/mod1\" pin 5).
    device:  \"/probe\"  (default)
             <I/O module name>, <clip module name> or <reference designator>
    pin:     <pin number>
    refpin:  <reference designator pin>  (default = \"\")")

    ("counter" .
     ":counter:
  counter device \"/mod4\", mode \"freq\" !set counter mode to freq on module 4
    device:  \"/probe\" (default), <I/O module name> or <clip module name>
             combinations of the above
    mode:    \"transition\" (default) or \"freq\"")

    ("cwd" .
     ":cwd:
  s = cwd()  !s is set to the current working directory
             !returns \"/userdisk/uut_name\", \"/userdisk/PODLIB/pod_name\"
             !or \"/userdisk/PROGLIB\"")

    ("dbquery" .
     ":dbquery:
  s = dbquery dbname            !get name of the compiled database
  s = dbquery expresp \"U29-11\", response \"rom0_data\"
     !get expected response for pin U29-11 from the response file rom0_data
  s = dbquery inputs \"U29-11\"   !get list of related inputs for U29-11
  s = dbquery node \"U29-11\"     !get list of pins on the node with U29-11
  s = dbquery npins \"U29\"       !get the number of pins on U29
  s = dbquery pintype \"U29-11\"  !get pin type of U29-11
  s = dbquery programs \"U29-11\" !get the names of all stimulus programs
                                !used to test pin U29-11")

    ("declare: (block form" .
     ":declare: (block form)
  declare
    numeric n
    numeric n2 = 3               !n2 is a numeric variable and initialized
                                 !to 3
    string s1 = \"hello\"          !s1 is a string and initialized to \"hello\"
    global numeric N             !N is a global numeric,  you MUST declare
                                 !N to be numeric and global in every
                                 !program and function using it.
    numeric array [1:10,1:20] n3 !numeric array n3 is ten by twenty
    string array [0:5,0:10] s    !string array s is six by eleven
    floating f = -3.14           !f is a floating point variable initialized
                                 !to -3.14
    persistent numeric n3        !n3 is a persistent numeric, you MUST
                                 !declare n3 to be numeric and persistent
                                 !in every program and function using it.
  end declare")

    ("declare: (statement form" .
     ":declare: (statement form)
  declare numeric n
  declare numeric n2 = 3        !n2 is a numeric variable and initialized
                                !to 3
  declare string s1 = \"hello\"   !s1 is a string and initialized to \"hello\"
  declare global numeric N      !N is a global numeric,  you MUST declare
                                !N to be numeric and global in every
                                !program and function it is used.
  declare numeric array [1:10,1:20] n3 !numeric array n3 is ten by twenty
  declare string array [0:5,0:10] s    !string array s is six by eleven
  declare floating f = -3.14    !f is a floating point variable initialized
                                !to -3.14
  declare persistent numeric n3 !n3 is a persistent numeric, you MUST
                                !declare n3 to be numeric and persistent
                                !in every program and function using it.
    default value: <default value for the variable>
                   (not allowed for global variables)
    array:  the array dimensions (surrounded by square brackets
            and separated by a \":\")")

    ("define menu" .
     ":define menu:
  define menu \"M1-C\", submenu \"M2\", label \"Other Tests\", key \"3\"
    !If key 3 is pressed for menu 1, submenu M2 (labeled \"Other Tests\")
    !is selected
    menu:     <menu name being defined>
    label:    <label for the menu item>
    key:      <key to be associated with the menu item>
    submenu:  <name of the submenu>")

    ("define mode" .
     ":define mode:
  define mode \"passed\", attribute \"blink,bold\"   !If the component passes,
           !it will have a bolded display and will blink
    mode:       <mode name being defined>
    attribute:  \"normal\", \"blink\", \"bold\" or \"inverse\"")

    ("define part" .
     ":define part:
  define part \"dip\", xdim 3, ydim 10, xdot 0, ydot 0
           !define a part named \"dip\" of size 3 x 10
           !with the dot in the upper, left-hand corner
    part:  <part name being defined>
    xdim:  <horizontal size of part>
    ydim:  <vertical size of part>
    xdot:  <horizontal location of alignment dot within part>
    ydot:  <vertical location of alignment dot within part>")

    ("define ref" .
     ":define ref:
  define ref \"U3\", xorg 1, yorg 1, part \"dip\", mode \"untested\"
    !define a ref named U3, using the part definition named \"dip\"
    !with its upper left-hand corner at location 1,1.  Use the display
    !mode defined for untested components.

    ref:   <reference designator name being defined>
    xorg:  <horizontal location of upper, left-hand corner>
    yorg:  <vertical location of upper, left-hand corner>
    part:  <name of a define part definition>
    mode:  <name of a defined display mode>")

    ("define text" .
     ":define text:
  define text \"U1\", label \"68000\", xorg 10, yorg 10, attribute \"bold\"
  !The bolded text \"68000\" is named U1 and located at 10,10
    text:       <text string name being defined>
    label:      <the text to be displayed>
    xorg:       <horizontal location for start of label>
    yorg:       <vertical location for start of label>
    attribute:  \"normal\", \"blink\", \"bold\" or \"inverse\"
                combinations of the above without spaces")

    ("delete" .
     ":delete:
  delete file \"wordlist\"  !text file named \"wordlist\" is removed from
                          !the disk
    file:  <text file name or full pathname>")

    ("diagnoseram" .
     ":diagnoseram:
  diagnoseram addr 0, upto $FFFE, mask $FF, fault_addr $2D34,
    data_expected $3B, data $37
    addr:          <starting address>
    upto:          <ending address>
    mask:          <bit mask for data bits to test>
    fault_addr:    <address of detected fault>
    data_expected: <data expected from fault address>
    data:          <data actually read from fault address>")

    ("diagnoserom" .
     ":diagnoserom:
  diagnoserom addr $E000, upto $EFFF, mask $F0F0, addrstep 2
    addr:          <starting address>
    upto:          <ending address>
    mask:          <bit mask for data bits to test>
                     (default = $FFFFFFFF)
    addrstep:      <address increment>")

    ("draw" .
     ":draw:
  draw channel ch3, xoff 10, yoff 15
    channel:  <numeric expression for channel>
    xoff:     <horizontal offset from xorg>
    yoff:     <vertical offset from yorg>")

    ("draw ref" .
     ":draw ref:
  draw ref \"U3,U5,U27\", channel ch3, xoff 10, yoff 15
    ref:      <list of defined reference designators>
    channel:  <numeric expression for channel>
    xoff:     <horizontal offset from xorg>
    yoff:     <vertical offset from yorg>")

    ("draw text" .
     ":draw text:
  draw text \"lab1,lab2\", channel ch3, xoff 10, yoff 15
    text:     <list of defined text names>
    channel:  <numeric expression for channel>
    xoff:     <horizontal offset from xorg>
    yoff:     <vertical offset from yorg>")

    ("drivepoll" .
     ":drivepoll:
  n = drivepoll device \"/mod1,/mod2\" !n is set to 1 when both are done
    device:  <I/O module name> or <clip module name> (default = \"/mod1\")
             combinations of the above")

    ("edge" .
     ":edge:
  edge device \"/mod2\", start \"+\", stop \"-\", clock \"+\"  !set edge levels
    device:  \"/probe\" (default), <I/O module name> or <clip module name>
             combinations of the above
    start:   \"+\" (default), \"-\" or \"at_arm\" (I/O module only)
    stop:    \"+\" (default), \"-\" or \"count\"
    clock:   \"+\" (default) or \"-\"")

    ("edgeoutput" .
     ":edgeoutput:
  edgeoutput device \"/mod1\", start \"+\", stop \"-\", clock \"+\" !set edge levels
    device:  <I/O module name> or <clip module name> (default = \"/mod1\")
             combinations of the above
    start:   \"+\" (default), \"-\" or \"at_vectordrive\"
    stop:    \"+\" (default) or \"-\"
    clock:   \"+\" (default) or \"-\"")

    ("edisk" .
     ":edisk:
  edisk load \"/HDR/DEMO\", vectorload \"on\", programload \"off\",
        objprogload \"off\", size 200
  ! create the E-disk
    load:        <UUT name>
    vectorload:  \"off\" (default) or \"on\"
    programload: \"off\" or \"on\" (default)
    objprogload: \"off\" or \"on\" (default)
    size:        <size of E-disk (in kbytes)>  (default = 500)
  edisk delete
  ! delete the E-disk")

    ("enable" .
     ":enable:
  enable device \"/mod1\", mode \"always\" !set I/O module input enable mode
    device:  \"/probe\" (default), <I/O module name> or <clip module name>
             combinations of the above
    mode (I/O module): \"always\" (default), \"high\", \"low\" or \"pod\"
    mode (probe): \"always\" (default), \"high\", \"low\", \"pod\", \"pod*en0\" or
             \"pod*en1\"")

    ("enableoutput" .
     ":enableoutput:
  enableoutput device \"/mod4\", mode \"high\" !set output clock enable mode
    device:  <I/O module name> or <clip module name> (default = \"/mod1\")
             combinations of the above
      mode:  \"always\" (default), \"high\" or \"low\"")

    ("end" .
     ":end:
  end declare     !end a declaration block
  end if          !end an if block
  end loop        !end a loop block
  end exercise    !end fault condition exerciser definition block
                  !  (this syntax is also OK: end <fault condition>)
  end handle      !end a fault condition handler definition block
                  !  (this syntax is also OK: end <fault condition>)
  end function    !end a function definition block
                  !  (this syntax is also OK: end <function name>)
  end program     !end a program
                  !  (this syntax is also OK: end <program name>)")

    ("execute" .
     ":execute:
  execute wrtio (10)  !execute the user-defined function named wrtio
                      !pass the number 10 to the function
  wrtio (10)          !execute the user-defined function named wrtio
                      !pass the number 10 to the function
  execute iotest (10) !execute the program named iotest and pass the
                      !number ten to the program
  n = max (8,10)      !execute the user-defined function named max")

    ("exercise" .
     ":exercise:
  exercise bad_data
    !program statements to exercise a user-written fault condition
    !exerciser for the fault condition bad_data
  end exercise !(or end bad_data  )")

    ("fabs" .
     ":fabs:
  f = fabs (x) !f is set to the absolute value of the floating-point value x")

    ("fails" .
     ":fails:
  if testbus addr $FFFE fails  then n = 0  !n is set to 0 if the bus is bad")

    ("fault" .
     ":fault:
  fault wrong_data  !generate a fault condition named wrong_data")

    ("filestat" .
     ":filestat:
  s = filestat file \"/HDR/ABC/LOGFILE\"  !get read/write status of a file
    file:   <text file name>            !returns \"rw-\"")

    ("for" .
     ":for:
  for n = 1 to 11 step 1
    !do this eleven times (n equals 1,2,3,4,5,6,7,8,9,10,11)
  next")

    ("fstr" .
     ":fstr:
  s = fstr (1.23E+02) !s is set to the string \"1.230000E+02\"")

    ("function" .
     ":function:
  function max (x,y) !max is an example of a user-defined function
    if (x > y) then
      return x       !x is returned since x is greater than y
    else
      return y       !y is returned since y is greater than or equal to x
    end if
  end max            !(or end function) this indicates the end of
                     !the max function")

    ("fval" .
     ":fval:
  f = fval (\"1.23\")  !f is set to the floating-point value 1.230000E+00")

    ("getoffset" .
     ":getoffset:
  n = getoffset device \"/probe\"  !n is set to the current offset amount
    device: \"/probe\"  (default) or <I/O module name>
    pin:    <pin number>  (default = 1)")

    ("getpod" .
     ":getpod:
  s = getpod podname    !s is set to the name of the current pod")

    ("getromsig" .
     ":getromsig:
  n = getromsig addr 0, upto $CF, mask $FF, addrstep 1 !get the signature
                                                       !for ROM
    addr:     <starting address>
    upto:     <ending address>
    mask:     <bit mask of data bits used to form signature>
              (default = $FFFFFFFF)
    addrstep: address increment")

    ("getspace" .
     ":getspace:
  n = getspace space \"memory\", size \"word\"
  !get the pod access number where \"memory\" is the value for the
  !parameter space and \"word\" is the value for the parameter size.
  !The parameter names and parameter values for getspace depend on the pod
  !you are using.  Refer to the Supplemental Pod Information for
  !9100A/9105A Users Manual.")

    ("gfi" .
     ":gfi:
  s = gfi accuse          !get GFI accusation string,
                          !e.g. \"U1 is BAD or OUTPUT U1-24 is LOADED\"
  gfi autostart \"enable\"  !start GFI automatically if GFI hints exist
     autostart: \"disable\" (default) or \"enable\"
  gfi clear               !clear GFI information
  s = gfi control         !if gfi is executing this program then s is
                          !set to \"yes\"
  s = gfi device          !get GFI testing device, e.g. \"/mod1\"
  gfi fail \"U9-2\"         !force GFI to fail \"U9-2\" for this program
  gfi hint \"AB-1\"         !add ref \"AB-1\" hint to the GFI hint list
  gfi pass \"U9-1\"         !force GFI to pass \"U9-1\" for this program
  s = gfi ref             !get GFI current reference designator
  s = gfi status \"U1-2\"   !get status of pin, e.g. \"good\", \"bad\" or
                          !\"untested\"
  s = gfi suggest         !get the next GFI suggestion, e.g. \"U1-2\"
  gfi test \"U2-1\", autoprompt \"no\"  !execute all the tests for \"U2-1\",
                          !the operator is not prompted
    autoprompt:  \"yes\" (default) or \"no\"")

    ("goto" .
     ":goto:
  goto test1    !transfer control to the line labeled test1")

    ("haltuut" .
     ":haltuut:
  haltuut ()    !halt runuut activity")

    ("handle" .
     ":handle:
  handle bit_tied_low
    !TL/1 statements to handle the bit_tied_low fault condition
  end bit_tied_low")

    ("ieee" .
     ":ieee:
  ieee channel ieeechn, timeout 1000, status $80, eoi \"on\", message \"remote\"
    channel:  <ieee channel number returned by open>
    message: <a string containing a special-purpose interface message or
              operation>
    eoi: <a string of \"on\" or \"off\">
    status: <a numeric value to assign to the serial poll status byte>
    timeout: <a numeric value which specifies the timeout interval in
              milliseconds>")

    ("if: (block form" .
     ":if: (block form)
  if n = 2 then
    !do this part only if the present value of n equals 2
  else if n = 3 then
    !do this part only if the present value of n equals 3
  else
    !do this part if the present value of n is not equal to 2 or 3
  end if")

    ("if: (statement form" .
     ":if: (statement form)
  if x = 10 then a = a + 1 \ b = b + 1   !Increment a and b if x=10")

    ("input" .
     ":input:
  input on ch2, n,b,r  !get values for n, b, and r from channel ch2
     !When entered, multiple values must be separated by space characters
  on:  <expression yielding a number for the channel
       to be used for input>
       (default = the first channel opened for \"input\" or \"update\")")

    ("input using" .
     ":input using:
  input using \"?%\", on ch2, s   !get a variable-width hexadecimal number
                                !from a channel named ch2
    on:  <expression yielding a number for the channel
         to be used for input>
         (default = the first channel opened for \"input\" or \"update\")")

    ("instr" .
     ":instr:
  n = instr (\"1234A6\",\"A\")  !n is set to 5")

    ("isflt" .
     ":isflt:
  n = isflt str s1   !n is set to 1 if fval(s1) would succeed, 0 otherwise")

    ("isval" .
     ":isval:
  n = isval str s1, radix 8  !n is set to 1 if val(s1,8) would succeed,
                             !0 otherwise
    str:    <the string to be evaluated>
    radix:  10 (default), 2, 8 or 16")

    ("len" .
     ":len:
  n = len (\"Be careful to count the spaces!\")  !n is set to 31")

    ("level" .
     ":level:
  n = level device \"/mod2\", type \"async\" , pin 1, refpin \"u1-1\"
  !n is set to async level of \"u1\" pin 1 (overriding \"/mod2\" pin 1).
    device:  \"/probe\" (default), <I/O module name>, <clip module name> or
            <reference designator>
    pin:    <pin number> (default = 1)
    type:   \"clocked\" (default) or \"async\"
    refpin: <reference designator pin>  (default = \"\")")

    ("loadblock" .
     ":loadblock:
  loadblock file \"F2\", format \"intel\", offset $FFFF2000 !loadblock command
                                                        !with an offset
    file:   <text file name containing the required data>
    format: \"motorola\" or \"intel\"
    offset: 32 bit 2's complement number added to load address
            (offset of $FFFF2000 changes load address from $F000 to $1000)")

    ("log" .
     ":log:
  f = log base 10.0, num 1000.0    !f is set to 3.000000E+00
    base: <Floating-point base number>
    num:  <Floating-point argument for the logarithm computation>")

    ("loop" .
     ":loop:
  loop for n = 1 to 17 step 2
    !do this nine times (n equals 1,3,5,7,9,11,13,15,17)
  end loop
  loop while n < 5
    !do this while n is less than 5
  end loop
  loop until n > 5
    !do this until n is greater than 5
  end loop")

    ("lsb" .
     ":lsb:
  n = lsb $280  !n is set to 7")

    ("mid" .
     ":mid:
  s = mid (\"Long text string\",13,4)  !s is set to the string \"ring\"
    str:     <string or string expression>
    from:    <integer expression>
    length:  <integer expression>")

    ("msb" .
     ":msb:
  n = msb $280  !n is set to 9")

    ("natural" .
     ":natural:
  f = natural pi     !f is set to 3.141593E+00
  f = natural e      !f is set to the value of e")

    ("next" .
     ":next:
  for n = 1 to 11 step 1
    !do this eleven times (n equals 1,2,3,4,5,6,7,8,9,10,11)
  next")

    ("open" .
     ":open:
  n = open device \"/port2\", as \"output\", speed 9600, bits 8, stop 1,
    parity \"none\", stall \"on\" , cts \"off\", mode \"unbuffered\",
    autolf \"on\", term \"\nl\"
           !open and setup a serial port channel
           !set n to the channel number
    device:
      <terminal name> \"/term1\"  (operator's display) or \"/term2\"  (monitor)
      <file name>     any text file name enclosed in quotes
      <port name>     \"/port1\" or \"/port2\"
      <window name>   \"/term1/win\"  \"/win\"  or \"/term2/win\"
    as:  \"input\", \"output\", \"update\" or \"append\"
         (default = \"update\" for terminal, window or port names)
         (default = \"append\" for file names)
    mode:  \"buffered\"  (default) or \"unbuffered\"
    term:  <termination character (a 0 or 1 character string)>")

    ("operators" .
     ":operators:
  n = 10 + 11  !n is set to 21
  s = \"k\" + \"en\"  !s is set to \"ken\"
  n = 10 - 1  !n is set to 9
  n = $20 * 2  !n is set to 64
  n = 10 * 10  !n is set to 100
  n = $100 / 2  !n is set to 128
  n = 56 / 3  !n is set to 18
  n = 56 % 3  !n is set to 2
  if 1 = 1  then n = 5  !since 1 equals 1, n is set to 5
  if 2 > 1  then n = 5  !since 2 is greater than 1, n is set to 5
  if 1 < 2  then n = 5  !since 1 is less than 2, n is set to 5
  if 1 <> 2 then n = 5  !since 1 is not equal to 2, n is set to 5
  if 1 <= 2 then n = 5  !since 1 is less than or equal to 2, n is set to 5
  if 2 >= 1 then n = 5  !since 2 greater than or equal to 1, n is set to 5
  n = $A and 3    !n is set to 2
  n = $A & 3      !n is set to 2
  s = \"X\" and \"1\" !s is set to \"X\"
  n = $10 or $20  !n is set to $30
  n = $10 | $20   !n is set to $30
  s = \"X\" or \"0\"  !s is set to \"X\"
  n = $1A xor $2C !n is set to $36
  n = $1A ^ $2C   !n is set to $36
  s = \"X\" xor \"0\" !s is set to \"X\"
  n = cpl $55     !n is set to $FFFFFFAA
  n = ~ $55       !n is set to $FFFFFFAA
  s = cpl \"X\"     !s is set to \"X\"
  n = not 1       !n is set to 0
  n = not 0       !n is set to 1")

    ("passes" .
     ":passes:
  if testbus addr $FFFE passes then n = 1  !n is set to 1 if the bus is OK")

    ("podinfo" .
     ":podinfo:
  n = podinfo addr       !n is set to the lowest valid address in
                         !the current space
  n = podinfo upto       !n is set to the highest valid address in the
                         !current space
  n = podinfo addrinc    !n is set to the minimum valid address increment
                         !for the current space
  n = podinfo datawidth  !n is set to the number of bits for data words
                         !in the current space
  n = podinfo datamask   !n is set to the bit mask for valid data bits in
                         !the current space
  n = podinfo addrmask   !n is set to the bitmask for valid address bits
                         !in the current space
  n = podinfo read       !n is set to 1 if a read is permitted
                         !in the current space; 0 otherwise
  n = podinfo write      !n is set to 1 if a write is permitted
                         !in the current space; 0 otherwise
  n = podinfo run        !n is set to 1 if run UUT is permitted
                         !in the current space; 0 otherwise
  n = podinfo busaddr    !n is set to the default bustest address")

    ("podsetup" .
     ":podsetup:
  podsetup 'report power' \"on\"     !report bad UUT power
  podsetup 'enable ~ready' \"on\"    !enable low-true ready signal
                                   !to control uP
  podsetup 'standby address' $8800 !setup standby address to $8800
  podsetup 'standby function on'   !begin standby read operations")

    ("poll" .
     ":poll:
  poll channel n, event \"input\"  !poll channel named n for input
    channel:  <an expression which identifies an open channel>
    event:    \"input\", \"output\", \"blocked\", \"errors\", \"break\", \"srq\"")

    ("pollbutton" .
     ":pollbutton:
  n = pollbutton ()  !n is set to 1 if I/O Module or Probe button has been
                     !pressed, else it is set to 0")

    ("polluut" .
     ":polluut:
  n = polluut () !n is set to 1 if the pod is in run uut, else it is
                 !set to 0")

    ("pow" .
     ":pow:
  f = pow num 2.0, power 3.0  !f is set to 8.000000E+00
    num:    <the floating-point number to raise to a power>
    power:  <the floating-point power argument>")

    ("pretestram" .
     ":pretestram:
  pretestram addr 0, upto $FFFE, mask $FFFF, addrstep 2
    addr:      <starting address>
    upto:      <ending address>
    mask:      <bit mask for data bits to test>
    addrstep:  <address increment>")

    ("print" .
     ":print:
  print 10,\"abc\"  !display the number 10 and string \"abc\" on operator
                  !display
    on:               <a numeric expression that identifies
                      a channel open for output>
                      (defaults to operators display if no output
                      channels have been opened)
    expression list:  <one or more expressions, separated by
                      commas>")

    ("print using" .
     ":print using:
  print using \"Hex number % = @@ decimal\nl\", on n, $A, $A
    !send the string \"Hex number A = 10 decimal\" to channel n
    on:               <a numeric expression that identifies
                      a channel open for output>
    expression list:  <one or more expressions, separated by
                      commas>")

    ("probe" .
     ":probe:
  probe ref \"A1-10\"  !request operator to probe pin 10 of \"A1\"")

    ("program" .
     ":program:
  program 'first.tl1' (arg1, arg2) !beginning of the program named first.tl1
                                   !Names with a period or numeric only
                                   !require 'single quotes'.
    <declarations>
    <fault condition handlers>
    <fault condition exercisers>
    <function definitions>
    <program commands>
  end program                      !(or end 'first.tl1') end of program")

    ("pulser" .
     ":pulser:
  pulser mode \"high\"  !set probe to pulse high
    mode:  \"off\" (default), \"high\", \"low\" or \"toggle\"")

    ("rampaddr" .
     ":rampaddr:
  rampaddr addr $4001, mask $F000  !ramp the address bits, all 0s to all 1s
    addr:     <address>
    mask:     <mask of address bits to be ramped>")

    ("rampdata" .
     ":rampdata:
  rampdata addr $2F, data $AA, mask $AF  !ramp the data bits, all 0s
                                         !to all 1s
    addr:     <address>
    data:     <first data value>
    mask:     <mask of data bits to be ramped>")

    ("random" .
     ":random:
  n = random ()    !n is set to a random 32-bit number
  n = random (103) !n is set to a random number, using 103 as the seed")

    ("read" .
     ":read:
  n = read addr $A2D2  !n is set to the data read at podspace address A2D2
    addr:     <address from which to read data>")

    ("readblock" .
     ":readblock:
  readblock file \"F1\", format \"motorola\", addr 0, upto $FF
          !store UUT memory in text file \"F1\"
    file:    <file name where data is to be stored>
    format:  \"motorola\" or \"intel\"
    addr:    <start address>
    upto:    <end address>")

    ("readbutton" .
     ":readbutton:
  s = readbutton mode \"beep\" !device is set to the device of
                             !the I/O Module or Probe button read
    mode:     \"beep\" or \"nobeep\" when the button is read")

    ("readdate" .
     ":readdate:
  s = readdate time systime()  !s is set to the present date, \"1987/08/19\"
    time:  <a number returned by the systime command>")

    ("readmenu" .
     ":readmenu:
  s = readmenu channel ch2, identifier \"M1\", name \"/term2\", style 1,
    xorg 10, yorg 20, height 15
    !s is set to a string indicating what menu item is selected
    channel:     <numeric expression for the input channel to read>
    identifier:  <menu name>
    name:        \"/term2\"
    style:       0  (default) or 1
    xorg:        <horizontal location of upper, left-hand
                 corner of menu>
    yorg:        <vertical location of upper, left-hand
                 corner of menu>
    height:      <maximum height of menu>")

    ("readout" .
     ":readout:
  readout device \"/mod3\"  !record responses from module 3 into memory
    device:  \"/probe\" (default), <I/O module name> or <clip module name>
             combinations of the above")

    ("readspecial" .
     ":readspecial:
  n = readspecial addr $1A2D2  !n is set to the data read at address 1A2D2
    addr:  <address from which to read data>")

    ("readstatus" .
     ":readstatus:
  n = readstatus ()  !n is set to the present status value")

    ("readtime" .
     ":readtime:
  s = readtime time systime()  !s is set to the present time, \"16:30:01\"
                               !It's time to go home!!!")

    ("readvirtual" .
     ":readvirtual:
  n = readvirtual extaddr $E, addr $1A2D2
  !n is set to the data read at 64 bit virtual address 0000000E0001A2D2
    extaddr: <upper 32 bits of virtual address>
    addr:    <lower 32 bits of virtual address>")

    ("readword" .
     ":readword:
  s = readword device \"/mod1\", word 1, mode \"now\"
    device:   <I/O module name> (default = \"/mod1\")
    word:     1-5 (default = 1)
    mode:     \"now\" (default) or \"stored\"")

    ("refault" .
     ":refault:
  refault pia_fault  !pass the pia_fault condition to the caller for
                     !further processing")

    ("remove" .
     ":remove:
  remove menu \"M1-B\"   !remove item B from menu M1
  remove menu \"M1\"     !remove the menu named \"M1\"
  remove mode \"passed\" !remove the mode definition named \"passed\"
  remove part \"box\"    !remove the part definition named \"box\"
  remove ref \"U42\"     !remove the ref definition named \"U42\"
  remove text \"J2\"     !remove the text definition named \"J2\"")

    ("reset" .
     ":reset:
  reset device \"/mod1,/mod2,/mod4\"  !reset modules 1,2 and 4 to initialize
    device:  \"/probe\" (default), <I/O module name> or <clip module name>
              combinations of the above")

    ("resetpersvars" .
     ":resetpersvars:
  resetpersvars()       ! empty the persistent variable set")

    ("restorecal" .
     ":restorecal:
  restorecal from \"USERDISK\", name \"/hdr\" !restore calibration values from
                                          ! /hdr USERDISK
  restorecal from \"UUT\", name \"ABC\"       !restore calibration values from
                                          ! the ABC UUT
  from:  <USERDISK or UUT>
  name:  <USERDISK or UUT name>   (Default = \"\" <current>)")

    ("return" .
     ":return:
  return      !returns control to the caller
  return x    !returns the value of x to the caller")

    ("rotate" .
     ":rotate:
  rotate addr $20, data $55  !the hex value 55 is rotated right at
                             !hex address 20
    addr:  <address for the write operations>
    data:  <initial data value>")

    ("runuut" .
     ":runuut:
  runuut addr 0, break $3F  !uP starts executing at address 0, stops at 3F
    addr:   <start address>
    break:  <break address>")

    ("runuutspecial" .
     ":runuutspecial:
  runuutspecial addr $FFFFFFFE, break $1000
      !uP starts executing at a special address
    addr:   <virtual start address>
    break:  <stop address>    (Default = 0)")

    ("runuutvirtual" .
     ":runuutvirtual:
  runuutvirtual extaddr $200, addr $FFFFFFFE, break $1000
  !uP starts executing at the 64 bit virtual address 00000200FFFFFFFE
    extaddr: <upper 32 bits of virtual start address>
    addr:    <lower 32 bits of virtual start address>
    break:   <stop address>    (Uses extaddr bits, Default = 0)")

    ("setbit" .
     ":setbit:
  n = setbit 8  !n is set to $100")

    ("setoffset" .
     ":setoffset:
  setoffset device \"/probe\", offset 1000010   !set 10 nsec past cal point
    device:   \"/probe\" (default) or <I/O module name>
    offset:   <offset value to be biased by 1000000> (default = 1000000)")

    ("setspace" .
     ":setspace:
  setspace space n  !pod address space is set to the specified access type
    space:  <numeric value returned by getspace or sysspace>")

    ("setword" .
     ":setword:
  setword device \"/mod1\", word 1, as_pins \"40 39 38 37 4 3 2 1\"
    device:   <I/O module name> (default = \"/mod1\")
    as_pin:   string of pin numbers (default = \"40 39 38 37 .... 4 3 2 1\")")

    ("shl" .
     ":shl:
  n = $A shl  !n is set to hex 14
  n = $A shl 3  !n is set to hex 50")

    ("shr" .
     ":shr:
  n = $A shr   !n is set to 5
  n = $A shr 3  !n is set to 1")

    ("sig" .
     ":sig:
  n = sig device \"/mod3B\", pin 10, refpin \"u1-1\"
  !n is set to u1-1 signature
  !n is set to crc of \"u1\" pin 1 (overriding \"/mod3B\" pin 10).
    device:  \"/probe\" (default), <I/O module name>, <clip module name> or
              <reference designator>
    pin:      <pin number>   (default = 1)
    refpin:   <reference designator pin>  (default = \"\")")

    ("sin" .
     ":sin:
  f = sin (theta)     !f is set to the sine of the variable theta")

    ("sqrt" .
     ":sqrt:
  f = sqrt (3.0)     !f is set to the square root of 3")

    ("stopcount" .
     ":stopcount:
  stopcount device \"/mod3\", count $F  !stop fifteen clocks after start edge
                                      !when edge stop \"count\" is used
    device:  \"/probe\" (default), <I/O module name> or <clip module name>
              combinations of the above
    count:    <numeric expression>")

    ("storepatt" .
     ":storepatt:
  storepatt device \"/mod2\", pin 3, patt \"1010101010101010\", refpin \"u1-1\"
  !store pattern for \"u1\" pin 1 (overriding \"/mod2\" pin 3).
    device:  <I/O module name>, <clip module name> or <reference designator>
    pin:     <pin number>
    patt:    <string composed of \"1\", \"0\", \"X\", or \"x\" characters>
    refpin:  <reference designator pin>  (default = \"\")")

    ("str" .
     ":str:
  s = str num 256, radix 16     !s is set to the string \"100\"
    num:    <numeric expression for number to be converted>
    radix:  10 (default), 2, 8 or 16")

    ("strobeclock" .
     ":strobeclock:
  strobeclock device \"/mod4\"  !clock module 4 sig and clocked level history
    device:  \"/probe\" (default), <I/O module name> or <clip module name>")

    ("strobeoutclock" .
     ":strobeoutclock:
  strobeoutclock device \"/mod4\"  !clock out next module 4 vector
    device:  <I/O module name> or <clip module name> (default = \"/mod1\")
             combinations of the above")

    ("sync" .
     ":sync:
  sync device \"/pod\", mode \"addr\"  !set pod to \"address synchronized\"
                                   !clocking
    device:   \"/pod\" (default), \"/probe\", <I/O module name> or
              <clip module name>
              combinations of the above
    mode:     (For probe, I/O module, or clip module devices)
                \"pod\", \"ext\", \"int\" or \"freerun\"
              (For probe only)
                \"capture\"
              (For pod device)
                Refer to the Supplemental Pod Information for 9100A/9105A
                Users Manual for the microprocessor you are using")

    ("syncoutput" .
     ":syncoutput:
  syncoutput device \"/mod1\", mode \"drclk\"  !set DRCLK to clock out vectors
    device:  <I/O module name> or <clip module name> (default = \"/mod1\")
              combinations of the above
    mode:     (For I/O module, or clip module devices)
                \"pod\" (default), \"drclk\", \"int\" or \"intfreq\"
              (For pod device)
                Refer to the Supplemental Pod Information for 9100A/9105A
                Users Manual for the microprocessor you are using")

    ("sysaddr" .
     ":sysaddr:
  n = sysaddr ()  !n is set to the last address written to or read from")

    ("sysdata" .
     ":sysdata:
  n = sysdata ()  !n is set to the last data read or written")

    ("sysinfo" .
     ":sysinfo:
  sysinfo get <attribute>
    attribute: \"/system/version\" or \"/system/model\"")

    ("sysspace" .
     ":sysspace:
  n = sysspace ()  !n is set to a number associated with the
                   !last address space accessed")

    ("systime" .
     ":systime:
  n = systime ()  !n is set to a numeric time/date value")

    ("tan" .
     ":tan:
  f = tan (theta) !f is set to the tangent of the variable theta")

    ("testbus" .
     ":testbus:
  testbus addr $FFFE   !the bus is tested, a write occurs at address FFFE
    addr:  <address>")

    ("testramfast" .
     ":testramfast:
  testramfast addr $1000, upto $3FFE, mask $FF, addrstep 2, delay 250,
      seed 1
    addr:       <starting address>
    upto:       <ending address>
    mask:       <bit mask for data bits to be tested>
                (default = $FFFFFFFF)
    addrstep:   <address increment>
    delay:      <milliseconds to delay>  (default = 250)
    seed:       <seed value>   (Default = 0)")

    ("testramfull" .
     ":testramfull:
  testramfull addr 0, upto $1FF, mask $FF, addrstep 2, delay 250,
    coupling \"off\"
    addr:       <starting address>
    upto:       <ending address>
    mask:       <bit mask for data bits to be tested>
                (default = $FFFFFFFF)
    addrstep:   <address increment>
    delay:      <milliseconds to delay>  (default = 250)
    coupling:   \"off\" (default) or \"on\"")

    ("testromfull" .
     ":testromfull:
  testromfull addr 0, upto $FFF, mask $FF, addrstep 2, sig $AE38
    addr:      <starting address>
    upto:      <ending address>
    mask:      <bit mask for data bits to be tested>
               (default = $FFFFFFFF)
    addrstep:  <address increment>
    sig:       <expected signature>")

    ("threshold" .
     ":threshold:
  threshold device \"/mod2\", level \"ttl\"  !set module 2 to sense TTL levels
    device:  \"/probe\" (default), <I/O module name> or <clip module name>
             <clip module name>
             combinations of the above
    level:   \"ttl\" (default), \"cmos\" or \"rs232\" (probe only)")

    ("toggleaddr" .
     ":toggleaddr:
  toggleaddr addr $1234, mask $46  !the bits set in the mask are toggled at
                                   !hex address 1234
    addr:  <address>
    mask:  <bit mask for address bits to toggle>")

    ("togglecontrol" .
     ":togglecontrol:
  togglecontrol ctl $A, mask 3 !the bits set in the mask are toggled in the
                               !control word $A
    ctl:   <control word>
    mask:  <Bit mask of control bits to toggle>")

    ("toggledata" .
     ":toggledata:
  toggledata addr $1F, data $33, mask $11  !the bits set in the mask are
                                           !toggled
    addr:  <address>
    data:  <data value>
    mask:  <bit mask of data bits to toggle>")

    ("token" .
     ":token:
  s = token str s1, seps \":;\", from 5      !set s to next token in s1
    str:  <the string to be scanned for a token>
    seps: <a string containing the set of token separator characters>
          (default = newline, linefeed, space and tab)
    from: <index in str to start scanning for token> (default=1)")

    ("val" .
     ":val:
  n = val str \"19\"              !n is set to the decimal number 19
  n = val str \"19\", radix 16    !n is set to the hex number 19
    str:     <a string that represents a number>
    radix:  10 (default), 2, 8 or 16")

    ("vectordrive" .
     ":vectordrive:
  vectordrive device \"/mod1,/mod2\", startmode \"at_arm\", vector 100
    device:  <I/O module name> or <clip module name> (default = \"/mod1\")
              combinations of the above
 startmode:   \"now\" (default) or \"at_arm\"
    vector:   1 (default) <first vector number>")

    ("vectorload" .
     ":vectorload:
  vectorload device \"/mod1,/mod2\", file \"vecfile\"
    device:   \"/mod1\"     (default)
              <I/O module name>
      file:   <name of vector file>")

    ("wait" .
     ":wait:
  wait time 10000  !wait for about 10,000 msec
    time:  <numeric expression for approximate wait time
           in milliseconds>")

    ("waituut" .
     ":waituut:
  waituut maxtime 100       !the execution waits for up to 100 msec for
                            !runuut to stop
    maxtime:  <maximum timeout value in milliseconds>")

    ("winctl" .
     ":winctl:
  winctl channel n, position \"front\"  !Move the window for channel n to the
                                      !front of the display
    channel:  <a channel for output to a window>
    position: \"front\", \"back\", \"hide\" or \"unhide\"")

    ("write" .
     ":write:
  write addr $100, data $AA !data AA is written at podspace address hex 100
    addr:  <address at which to write>
    data:  <data to write>")

    ("writeblock" .
     ":writeblock:
  writeblock file \"F1\", format \"motorola\"  !file \"F1\" provides data to be
                                           !written into UUT memory
    file:    <text file name containing the required data>
    format:  \"motorola\" or \"intel\"")

    ("writecontrol" .
     ":writecontrol:
  writecontrol ctl $04  !control lines are written with the data 04
    ctl:  <control word>")

    ("writefill" .
     ":writefill:
  writefill addr 0, upto $3FE, data $C7 !C7 is written into addresses 0-3FE
    addr:  <starting address>
    upto:  <ending address>
    data:  <data value>")

    ("writepatt" .
     ":writepatt:
  writepatt device \"/mod2\", mode \"pulse\"  !write pulsed pattern to module 2
    device:  <I/O module name>, <clip module name> or <reference designator>
             (default = \"/mod1\")
             combinations of the above
    mode:    \"latch\" (default) or \"pulse\"")

    ("writepin" .
     ":writepin:
  writepin device \"/mod1\", pin 21, level \"1\", mode \"latch\", refpin \"u1-1\"
  !write a 1 (HIGH) to \"u1\" pin 1 (overriding \"/mod1\" pin 21) and latch it.
    device:  <I/O module name>, <clip module name> or <reference designator>
             (default = \"/mod1\")
    pin:     <I/O module pin number>  (default = 1)
    level:   \"0\" (default), \"1\", \"X\" or \"x\"
    mode:    \"latch\" (default) or \"pulse\"
    refpin:  <reference designator pin>  (default = \"\")")

    ("writespecial" .
     ":writespecial:
  writespecial addr $100, data $3C !data 3C is written at hex address 100
    addr:  <virtual address where data will be written>
    data:  <data to write>")

    ("writevirtual" .
     ":writevirtual:
  writevirtual extaddr $A, addr $100, data $3C
 !data 3C is written to 64 bit virtual address 0000000A00000100
    extaddr: <upper 32 bits of virtual address>
    addr:    <lower 32 bits of virtual address>
    data:    <data to write>")

    ("writeword" .
     ":writeword:
  writeword device \"/mod1\", word 1, patt \"01010101XX\"
  !write word 1 with \"01010101XX\"
    device:  <I/O module name> (default = \"/mod1\")
    word:    1-5 (default = 1)
    patt:    string of X, 0, and 1"))

  "Documentation for every function in TL/1.")

(defconst tl1-mode--builtin-re
  (tl1-mode--word-opt
   (cl-remove-if (lambda (builtin)
                   (seq-contains tl1-mode--keywords builtin))
                 (mapcar #'car tl1-mode--function-documentation)))
  "TL/1 built-in functions.")

(defconst tl1-mode--operator-re
  (tl1-mode--word-opt '("and" "or" "xor" "shl" "shr" "cpl" "setbit"
                        "msb" "lsb" "bitmask" "len" "not"))
  "TL/1 built-in operators.")

(defconst tl1-mode--type-re
  (tl1-mode--word-opt
   '("numeric" "floating" "string" "array"))
  "A list of all types known to TL/1.")

(defconst tl1-mode--font-lock-keywords
  `(
    ;; Built-ins
    (,tl1-mode--builtin-re . font-lock-builtin-face)
    (,tl1-mode--operator-re . font-lock-builtin-face)

    (,tl1-mode--keyword-re . font-lock-keyword-face)

    ;; Programs & functions
    ("\\b\\(function\\|program\\)\\s-+\\([a-z0-9_]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ;; Types
    (,tl1-mode--type-re . font-lock-type-face)

    ;; Variables
    ("\\b\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-+=" (1 font-lock-variable-name-face))

    ;; Constants
    ("\\b\\([0-9]+\\|\$[0-9a-f]+\\)" . font-lock-constant-face))

  "Font-locking defintions for ‘tl1-mode’.")

(defconst tl1-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; A `!' is the start of a comment
    (modify-syntax-entry ?! "<" table)
    ;; The comment extends to the end of the line the `!' appears on.
    (modify-syntax-entry ?\n ">" table)
    table))

(defun tl1-mode--line-blankp ()
  (re-search-forward "^\\s-*$" (line-end-position) t))

(defun tl1-mode--skipback ()
  "Move backwards to the first non-blank line, or beginning of buffer."
  (while (progn (forward-line -1)
                (and (not (bobp))
                     (or
                      (looking-at "^\\s-*$")
                      (looking-at "^\\s-*!.*$"))))))

(defun tl1-mode--line-type ()
  "Determine the type of the current line.

   Lines may be:
     :START     -- the beginning of an indented block.
     :END       -- the end of an indented block.
     :START-END -- End of the previous block, and the start of a new one.
     NIL        -- Not a block member, a normal statement."
  (cond
   ;; Empty line
   ((tl1-mode--line-blankp) nil)

   ;; Start of a block
   ((or (looking-at "^\\s-*\\(program\\|function\\|loop\\|handle\\|exercise\\)")
        (looking-at "^\\s-*declare\\s-*\\(!.*\\)?$")
        (looking-at "^\\s-*if.*then\\s-*\\(!.*\\)?$")
        (looking-at "^\\s-*arm\\s-+device\\s-+")
        (looking-at "^\\s-*for\\s-+"))
    :start)

   ;; End of one block, start of another.
   ((looking-at "^\\s-*else") :start-end)

   ;; End of a block
   ((or (looking-at "^\\s-*end")
        (looking-at "^\\s-*next")
        (looking-at "^\\s-*readout\\s-+device\\s-+"))
    :end)

   ;; Any other line
   (t nil)))

(defun tl1-mode-indent ()
  "Indent line for TL/1 mode."
  (interactive)
  (save-excursion
    (indent-line-to
     (save-match-data
       (let* ((case-fold-search t)
              (block (save-excursion (goto-char (line-beginning-position))
                                     (tl1-mode--line-type)))
              (last-line (save-excursion (tl1-mode--skipback)
                                         (cons
                                          (current-indentation)
                                          (tl1-mode--line-type))))
              (last-indent (car last-line))
              (last-line-type (cdr last-line)))

         (cond
          ;; Left-aligned at start of buffer
          ((bobp) 0)

          ((or
            ;; Normal statements align together
            (and (not block) (not last-line-type))

            ;; Blocks align together
            (and (eq :start block)
                 (eq :end last-line-type))

            (and (eq :end block)
                 (eq :start-end last-line-type)))

           last-indent)

          ((eq block :start-end) (- last-indent tab-width))

          ;; This line is the end of a block, last was start
          ((and (eq :end block)
                (eq :start last-line-type))
           last-indent)

          ;; Last line was start of a block
          ((or (eq :start last-line-type)
               (eq :start-end last-line-type))
           (+ last-indent tab-width))

          ;; This line is the end of a block
          ((eq block :end) (- last-indent tab-width))

          ;; Catch-all
          (t last-indent))))))

  ;; If the line is blank, move point to the end of it.
  (when (tl1-mode--line-blankp)
    (goto-char (line-end-position))))

;;;###autoload
(define-derived-mode tl1-mode prog-mode "TL/1"
  "Major mode for editing Fluke TL/1 source code."
  (setq tab-width 4)
  (setq tab-stop-list '(2 0))
  (setq tab-always-indent t)

  (set-syntax-table tl1-mode--syntax-table)
  (set (make-local-variable 'comment-start) "!")
  (setq comment-use-syntax t)
  (setq fill-prefix nil)
  (setq indent-line-function #'tl1-mode-indent)

  (setq font-lock-defaults '(tl1-mode--font-lock-keywords nil nil)))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons "\\.tl1\\'" 'tl1-mode))


;;;; ElDoc support

(defun tl1-mode--eldoc-summary (doc)
  "Summarize the documentation DOC."
  (cadr (split-string doc "\n" nil "\\s-+")))

(defun tl1-mode--eldoc-fontify (doc)
  "Return fontified DOC."
  (with-temp-buffer
    (tl1-mode)
    (insert doc)
    (font-lock-ensure (point-min) (point-max))
    (buffer-substring (point-min) (point-max))))

(defun tl1-mode--eldoc-function-at-point ()
  "Return the TL/1 function at point."
  (save-excursion
    (save-match-data
      (thing-at-point 'word))))

(defun tl1-mode--eldoc-documentation-function ()
  "Return documentation for the TL/1 function at point."
  (save-excursion
    (save-match-data
      (if-let ((doc (cdr (assoc (tl1-mode--eldoc-function-at-point) tl1-mode--function-documentation))))
          (tl1-mode--eldoc-fontify
           (if (eq t eldoc-echo-area-use-multiline-p) doc
             (tl1-mode--eldoc-summary doc)))))))

;;;###autoload
(defun tl1-mode-eldoc-enable ()
  "Enable ElDoc support in TL/1 mode."
  (interactive)
  (add-function :before-until (local 'eldoc-documentation-function)
                #'tl1-mode--eldoc-documentation-function))

(provide 'tl1-mode)
;;; tl1-mode.el ends here
