Verse
=====

Simple 2D physics engine wrapped in a game (work in progress). Written in Common Lisp. OpenGL graphics.

Set up
* Get SBCL:  http://sbcl.org
* Get Quicklisp: http://www.quicklisp.org/beta/
* Get GLFW (libglfw): http://www.glfw.org 

Run

    ./run.sh
    
    or
    
    sbcl --load client.lisp --eval "(verse::game)" --quit
    
Keys

    A - Turn left
    D - Turh right
    Right shift - Thrust
    P - Pause
    F1 - reset
    F2 - toggle fullscreen
    1 and 2 - forward / rewind states while in pause
