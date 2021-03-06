(defun memacs-splash (load-time)
  (switch-to-buffer "*MeMacs Splash*")
  (insert "\n\n\n")
  (insert "            _ _           _             _             \n")
  (insert "  /\\  /\\___| | | ___     /_\\  _ __   __| |_ __ _____      __ \n")
  (insert " / /_/ / _ \\ | |/ _ \\   //_\\\\| '_ \\ / _` | '__/ _ \\ \\ /\\ / / \n")
  (insert "/ __  /  __/ | | (_) | /  _  \\ | | | (_| | | |  __/\\ V  V /  \n")
  (insert "\\/ /_/ \\___|_|_|\\___/  \\_/ \\_/_| |_|\\__,_|_|  \\___| \\_/\\_/   \n")
  (insert "\n\n\n")
  (insert (format "Load time: %s" load-time)))
	
