(ql:quickload :birch)

(defclass my-connection (birch:connection) ())

(defvar *freenode* (make-instance 'my-connection
                                  :server-host "chat.freenode.net"
                                  :nick "nymphet-bot"))
(birch:connect *freenode*)
(birch:/join *freenode* "#lainchan")

(birch:/privmsg *freenode* "loli" "test")
(birch:handle-message *freenode* "rar")
