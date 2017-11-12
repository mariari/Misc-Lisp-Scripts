This is just a bunch of Miscellaneous code I've played around with.
the asd file loads functions that are important to other functions in the project, for example finger-tree.lisp relies on lazy-struct which relies on lazy-list which the asd file loads for you.

Some notable files are
1. `threadtest.lisp` - parallel mapcar, and a S! macro that injects thread locking to a critical section of code
2. `data-structures` - this has a few data structures I've decided to make
3. `numberConverter.lisp` - a simple little program that converts numbers to different redixes, made when I was first learning lisp

