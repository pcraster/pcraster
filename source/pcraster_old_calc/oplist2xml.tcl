#!/bin/env tclsh

# \
exec tclsh "$0"

if { $tcl_version < 8.0 } {
  error "need newer release of tcl"
  exit 1
}

# set a parameter to $value if it did not exist
# or incr with $value it does exist
proc IncrSet { varName { value 1 } } {
  upvar $varName var
  if { ![info exists var ]} {
    set var 0
  }
  set var [ expr $var+$value]
}

proc assert { args } {
  global errorInfo errorCode
  set code [ catch { uplevel $args } string ]
  if { $code == 1 } {
   return -code error -errorinfo $errorInfo \
     -errorcode $errorCode $string
  } elseif { $code == 0 } {
# test result of body
     if { $string == 0 } { 
       error "assert" "assertion $args failed"
       exit 1
     } else {
       return
     }
  } else {
# code is 2,3,4 or user defined
       error "assert" "assertion body: '$args' returned wrong code: $code "
       exit 1
  }
  return
}

#  script contains definitions of the operators and functions
#  and code to create the checking structures in "operator.inc" 
#  and MAJOR_CODE codes in "major_op.h"
#  etc. all created files are opened here:
#

proc Comment { fd msg } {
 puts $fd "/* $msg */"
}

proc CreateFile { fName descr } {
 set fd [ open $fName "w" ]
 Comment $fd "this file is created by oplist. do not edit"
 Comment $fd "$descr"
 return $fd

}

# find for a dass-operator it's other operator
proc AddToDassTable { dassId opCode } {
 # dassId is the CG_ field with an |0 attached
 # it is always as CG_PLAIN

 if { ! [ string match *|0 $dassId ] } {
   return $dassId
 }
 # depends on tcl-switch that only the first match is evaluated
 set op2 ""
 switch -glob -- $opCode {
     *FLUX      { regsub FLUX $opCode  "STATE" op2 }
     *STATE     { regsub STATE $opCode "FLUX"  op2 }
     *ZONE      { regsub ZONE  $opCode ""      op2 }
     *DEM       { regsub DEM   $opCode ""      op2 }
     OP_SPREAD*    { set op2  "${opCode}ZONE" }
     OP_LDDCREATE  { set op2  "OP_LDDCREATEDEM" }
     OP_DYNAMICWAVEQ  { set op2  "OP_DYNAMICWAVEH" }
     OP_DYNAMICWAVEH  { set op2  "OP_DYNAMICWAVEQ" }
 }
 global dassFd
 puts $dassFd "\{$opCode,$op2\},"
 return "CG_PLAIN"
}

proc DassJmp { theFunc opName } {
  set op1 ""
  set op2 ""
  switch -glob -- $opName {
   *ACCU* {
    set op1 "${opName}STATE"
    set op2 "${opName}FLUX"
   }
   *DIFFU* {
    set op1 "${opName}STATE"
    set op2 "${opName}FLUX"
   }
   *DYNAMICWAVE* {
    set op1 "${opName}Q"
    set op2 "${opName}H"
   }
  *_IMPL* {
            regsub _IMPL $opName "" opName
            switch -glob -- $opName {
             *SPREAD* {
              set op1 $opName
              set op2 "${opName}ZONE"
              }
             *LDDCREATE* {
              set op1 $opName
              set op2 "${opName}DEM"
             }
          }
         }
  }
  if { $op1 == "" } {
     error "DassJmp Failed"
  }
  global dassJmp
  puts $dassJmp "\{\{$op1,$op2\},$theFunc\},"
}

# map code to implementor and index of implementor
proc AddToDoubleTable { opCode } {
 set dassCode $opCode
 set i -1
 switch -glob -- $opCode {
   *STATE {
     set i 0
     regsub STATE $opCode "" dassCode
   }
   *DYNAMICWAVEQ {
     set i 0
     regsub Q $opCode "" dassCode
   }
   *DYNAMICWAVEH {
     set i 1
     regsub H $opCode "" dassCode
   }
   *FLUX {
     set i 1
     regsub FLUX $opCode "" dassCode
   }
   *ZONE {
     set i 1
     regsub ZONE $opCode "_IMPL" dassCode
   }
   *EAD*  {
     set i 0
     set dassCode "${dassCode}_IMPL"
   }
   *LDDCREATEDEM* {
     set i 1
     set dassCode "OP_LDDCREATE_IMPL"
   }
   *LDDCREATE* {
     set i 0
     set dassCode "OP_LDDCREATE_IMPL"
     }
 }
 global doubleFd
 assert expr $i == 0 || $i == 1
 puts $doubleFd "/* $opCode */\{ $dassCode, $i\},"
}

set opFd [ CreateFile operator.inc "check table for parsing rules" ]
set dassFd [ CreateFile dassop.inc "pairs of dass functions" ]
set doubleFd [ CreateFile double.inc "DASS operator belong to DOUBLE" ]

# set assFd [ CreateFile ass.inc "operator belong to EXEC_ASS (+=, etc )" ]

set majorOp [ CreateFile major_op.h "major opcodes" ]
set sunJmp [ CreateFile sunjmp.inc "EXEC_SAME_UN jump table node" ]
set dunJmp [ CreateFile dunjmp.inc "EXEC_DIFF_UN jump table node" ]
set sbinJmp [ CreateFile sbinjmp.inc "EXEC_SAME_BIN jump table node" ]
set dbinJmp [ CreateFile dbinjmp.inc "EXEC_DIFF_BIN jump table node" ]
set ifthenJmp [ CreateFile ifthenjmp.inc "EXEC_IFTHEN jump table node" ]
set ifthenelseJmp [ CreateFile ifthenelsejmp.inc "EXEC_IFTHENELSE jump table node" ]
set globJmp [ CreateFile globjmp.inc "EXEC_GLOBAL jump table node" ]
set dassJmp [ CreateFile dassjmp.inc "EXEC_DASS jump table node" ]
Comment $dassJmp "fluxes and (spread)zones are always second"
set genJmp [ CreateFile genjmp.inc "EXEC_GEN_NS jump table node" ]
set miscJmp [ CreateFile miscjmp.inc "EXEC_MISC jump table node" ]
set toutJmp [ CreateFile toutjmp.inc "EXEC_TOUT jump table node" ]
set polyJmp [ CreateFile polyjmp.inc "EXEC_POLY jump table node" ]
set trigJmp [ CreateFile trigjmp.inc "EXEC_TRIG jump table node" ]

set nrDefines 0
set nrFuncs 0
set nrPolys 0
set nrTrigs 0

# read on line from fd and put each item in list
# assuming items on lines are seperated by space
# returns number of items, values put in listName
proc GetList { fd listName { get3 0 } } {
  upvar $listName l
  set len 0
  while { $len == 0 } {
   if { [ gets $fd line ] == -1 } {
    return -1;
   }
   if { $get3 } {
     # expect 3th arg after #
     regsub "#" $line "" result
   } else {
     regsub "#.*$" $line "" result
 }
   set l [ concat $result ]
   set len [ llength $l ]
   if { $len == 0 } {
    puts "<!-- $line -->"
   }
  }
  return $len
}

proc Spatial { str } {
 regsub "ST_" $str "" field
 switch -- $field {
  "DERIVED" {
   return ""
  }
  "NONSPATIAL" {
   return " spatial='non'"
  }
  "SPATIAL" {
   return " spatial='yes'"
  }
 }
}


set inputFd [ open oplist.txt "r" ]
while { 1 } {
 set nr [ GetList $inputFd line ]
 if { $nr == -1 } {
  break
 }

 # descr should have  7 or 8 fields
 assert expr $nr != 7 || $nr != 8

 # put in nr array to mimic old awk-script
 set i 1
 foreach a $line {
  set descr($i) [ lindex $line [ expr $i - 1 ] ]
  incr i
 }

 puts "<Operation name='$descr(1)'"
 # if (NF == 8) then $1 is not a valid character string
 #   $8 is MNENOMIC
 #   that also means it is an implementor?
 set op $descr(1)
 if { $nr == 8 } {
  puts "           implementationName='$descr(8)'"
 }

 # syntax SYNTAX_FUNC or SYNTAX_OP
 if { $descr(2) == "SYNTAX_OP" } {
  puts "           syntax='Operator'"
 }
 if { $descr(2) == "SYNTAX_NONE" } {
  puts "           syntax='Internal'"
 }

 # 7 is the is the CG_ field with an |0 attached
 switch -glob -- $descr(7) {
    *COMM   { puts "           cg='COMM'"}
    *VARARG { puts "           cg='VARARG'"}
    *SPEC   { puts "           cg='SPEC'"}
 }
 set multResult [ string match *|0 $descr(7) ]
 
 # Exec
 regsub "EXEC_" $descr(6) "" funcType
 puts "           exec='$funcType'>"

 if [ string match *|0 $descr(7) ] {
  puts " <MultipleResultFunction/>"
 }

## RESULT STUFF
 # valuescale
 set vs $descr(3)
 # field (ST_SPATIAL|ST_NONSPATIAL|ST_DERIVED)
 set field [ Spatial $descr(4) ]
 puts " <Result><Field${field}>&$descr(3);</Field></Result>"
 set varArg [ expr $descr(5) < 0 ]
 set nrArgs [ expr abs($descr(5)) ]
 for { set i 1 } { $i <= $nrArgs } { incr i } {
   set nrInA [ GetList $inputFd line 1 ]
   set vs [ lindex $line 0 ]
   set v ""
   if [ expr $varArg && $i == $nrArgs ] {
     set v " repeat='true'"
   }
   set un ""
   if { [ llength $line ] > 2 } {
      set un " contextName='[lindex $line 2]'"
   }
   set field [ Spatial [ lindex $line 1 ] ]
   puts " <FieldArg$v$un><Field$field>&$vs;</Field></FieldArg>"
 }
 puts "</Operation>"
}
exit 

# end of while
# input file is now written

puts $majorOp "#ifndef INCLUDED_MAJOR_OP"
puts $majorOp "#define INCLUDED_MAJOR_OP"
puts $majorOp "typedef enum MAJOR_CODE \{ "

puts $opFd "\n\nstatic const calc::Operator parsArgTable\[\] = \{"

for {set i 0 } { $i < $nrDefines } { incr i } {
 # set f to be function prototype e.g. 
 # (EXEC_SAME_UN)func -> (DO_SAME_UN)func
 regsub "EXEC" $doFuncs($i) "DO" f
 switch -glob -- $f {
     (DO_SAME_UN* { puts $sunJmp "$f," }
     (DO_DIFF_UN* { puts $dunJmp "$f," }
     (DO_GLOB*    { puts $globJmp "$f," }
     (DO_GEN_NS*  { puts $genJmp "$f," }
     (DO_MISC*    { puts $miscJmp "$f," }
     (DO_TOUT*    { puts $toutJmp "$f," }
     (DO_SAME_BIN* {
          puts $sbinJmp "\{${f}_ss,${f}_ns,${f}_sn\}," 
  }
     (DO_DIFF_BIN* {
          puts $dbinJmp "\{${f}_ss,${f}_ns,${f}_sn\}," 
       }
     (DO_IFTHEN)* {
          puts $ifthenJmp "\{${f}_ss,${f}_ns,${f}_sn\}," 
       }
     (DO_IFTHENELSE* {
      puts $ifthenelseJmp \
 "\{${f}_sss,${f}_nns,${f}_nsn,${f}_nss,${f}_snn,${f}_sns,${f}_ssn,${f}_sss\},"
  }
     (DO_DASS* {
        DassJmp $f $defines($i)
     }
 }

 # last item in structures?
 set end ","
 if { $i == [ expr $nrDefines-1 ] } {
   set end ""
 }
 # write major and operator info
 puts $majorOp "$defines($i)=$i $end"
 puts $opFd " $table($i) $end"

}
# end for-loop defines

# terminate
puts $opFd    "\};"
puts $majorOp "\} MAJOR_CODE ; "
puts $majorOp "#endif /* INCLUDED_MAJOR_OP */"


# print the func table, sorted on name

puts $opFd "/* maps a name to a major code */"
puts $opFd "typedef struct FUNC_ID {"
puts $opFd " const char *name;  /* name of function   */"
puts $opFd "  MAJOR_CODE  major; /* major number of function */"
puts $opFd "} FUNC_ID;"

for { set i 0 } { $i < $nrFuncs } { incr i } {
  lappend funcListIn $funcTable($i)
}
set funcListSorted [ lsort $funcListIn ]
set i 1
puts $opFd "\n\nstatic const FUNC_ID stdFuncTable\[\] = \{"
foreach a $funcListSorted {
   set endOp    ","
   if { $i == $nrFuncs } {
     set endOp    "\};"
  }
  puts $opFd "$a $endOp"
  incr i
}

puts $opFd "\n\nstatic size_t nrInternalOpCodes = $nrDefines;\n"

# create the polyJmp table
#  UINT1 is not defined for every poly-function
#  create list with this defined, fetch all *_1 patterns
foreach { name value } [ array get defines ] {
  if { [ string match *_1 $value ] } {
    lappend uint1Polys $value
#    puts "--> $value"
  }
}
for { set i 0 } { $i < $nrPolys } { incr i } {
   set o $poly($i)
   set t1 "${o}_1"
#   puts "<-- $o "
   if { [ lsearch -exact $uint1Polys $t1 ] == -1 } {
      set t1 "OP_NOP"
   }
   puts $polyJmp "\{ $t1,${o}_4,${o}_S\},"
}

for { set i 0 } { $i < $nrTrigs } { incr i } {
   set o $trig($i)
   puts $trigJmp "\{ ${o}_D,${o}_S\},"
}
