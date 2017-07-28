[WHB04B_BUTTONS]
# use button names according to layout file LIB:whb04b-layout{n}.cfg
# note: "start-pause" is connected for standard behavior
#        controlling halui.pause/halui.resume/halui.run
# these are examples, edit as required:

#goto-zero   = halui.mdi-command-00
# synthesized per-axis buttons for goto-zero button:
#goto-zero-x = ""
#goto-zero-y = ""
#goto-zero-z = ""
#goto-zero-a = ""
#goto-zero-b = ""
#goto-zero-c = ""

start-pause = std_start_pause
stop-rewind      = halui.program.stop
# probe-input for simulating a probe:
feedplus-out3     = motion.probe-input
feedminus-out4    = halui.mdi-command-03
spindleplus-out5  = ""
spindleminus-out6 = ""
morigin-out7 = ""
half-safez = ""
worigin-probez = ""
switchccordinate =""

half        = halui.spindle.stop
# synthesized per-axis buttons for half button:
#half-x      = ""
#half-y      = ""
#half-z      = ""
#half-a      = ""
#half-b      = ""
#half-c      = ""

#zero        = ""
# synthesized per-axis buttons for zero button:
#zero-x      = halui.mdi-command-12
#zero-y      = halui.mdi-command-13
#zero-z      = halui.mdi-command-14
#zero-a      = halui.mdi-command-15
#zero-b      = halui.mdi-command-16
#zero-c      = halui.mdi-command-17

#safe-z      = halui.mdi-command-10
# in this sim, home-all only works first time, hold down till finished:
#home        = halui.home-all
#out-3     = halui.mdi-command-01
#macro-2     = halui.mdi-command-02
spindleonoff     = halui.spindle.start
#step        = whb04b.stepsize-up
#mode        = ""
#macro-6     = halui.mdi-command-06
#macro-7     = halui.mdi-command-07
#stop        = halui.program.stop
reset       = halui.estop.activate

[HALUI]
# these are examples, edit as required:
# a halui hal pin is created for each MDI_COMMAND below
# halui.mdi-command-00, halui.mdi-command-01,... etc
MDI_COMMAND=G0 X0 Y0 Z0
MDI_COMMAND=(debug, example: mdi-01)
MDI_COMMAND=(debug, example: mdi-02)
MDI_COMMAND=(debug, example: mdi-03)
MDI_COMMAND=(debug, example: mdi-04)
MDI_COMMAND=(debug, example: mdi-05)
MDI_COMMAND=(debug, example: mdi-06)
MDI_COMMAND=(debug, example: mdi-07)
MDI_COMMAND=(debug, example: mdi-08)
MDI_COMMAND=(debug, example: mdi-09)
MDI_COMMAND=(debug, example: mdi-10)
MDI_COMMAND=(debug, example: mdi-11)
#12
MDI_COMMAND= G10 L20 P0 X0
#13
MDI_COMMAND= G10 L20 P0 Y0
#14
MDI_COMMAND= G10 L20 P0 Z0
#15
MDI_COMMAND= G10 L20 P0 C0
#16
MDI_COMMAND= G10 L20 P0 Z0
#17
MDI_COMMAND= G10 L20 P0 C0

