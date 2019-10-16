add_library(pdcurses STATIC
    addch.c
    addchstr.c
    addstr.c
    attr.c
    beep.c
    bkgd.c
    border.c
    clear.c
    color.c
    debug.c
    delch.c
    deleteln.c
    getch.c
    getstr.c
    getyx.c
    inch.c
    inchstr.c
    initscr.c
    inopts.c
    insch.c
    insstr.c
    instr.c
    kernel.c
    keyname.c
    mouse.c
    move.c
    outopts.c
    overlay.c
    pad.c
    panel.c
    pdcclip.c
    pdcdisp.c
    pdcgetsc.c
    pdckbd.c
    pdcscrn.c
    pdcsetsc.c
    pdcutil.c
    printw.c
    refresh.c
    scanw.c
    scr_dump.c
    scroll.c
    slk.c
    termattr.c
    touch.c
    util.c
    window.c
)

add_library(pcraster::pdcurses ALIAS pdcurses)


target_include_directories(pdcurses
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

#  WIDE=Y UTF8=Y
target_compile_definitions(pdcurses
    PRIVATE
         -DPDC_WIDE
         -DPDC_FORCE_UTF8
)
# -DPDC_DLL_BUILD shouldn't be necessary...
# debug: -Z7 -DPDCDEBUG
