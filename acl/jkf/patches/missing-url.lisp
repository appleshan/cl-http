(in-package :url)

;;; this is called but in the complete sources there is no definition

(defun newer-update-urls (oldhost newhost &key old-port newport)
  (warn "I am undefined ~a ~a ~a ~a~%" oldhost newhost old-port newport))

;;; definition was wrong, variable new-port is spelled newport
(defun update-urls-host-port (newhost newport oldhost oldport &optional (local-host-ip-number (www-utils:local-host)))
  (newer-update-urls oldhost newhost :old-port oldport :new-port newport)
  (update-urls (make-local-context newhost newport) (make-local-context oldhost oldport) local-host-ip-number))