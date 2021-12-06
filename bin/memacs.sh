ACTION=$1

START="(progn "
MESSAGE_DONE="(message \"Done!\")"
KILL_EMACS="(kill-emacs)"
END=")"

if [ $ACTION == 'install' ]
then
    LOADING_MESSAGE="(message \"Loading Core...\")"
    LOAD_CORE="(load \"~/.emacs.d/core.el\")"
    INSTALL_MESSAGE="(message \"Installing Core Packages\")"
    INSTALL_CORE="(install-core)"
    INSTALL="$START 
              $LOADING_MESSAGE
              $LOAD_CORE
              $INSTALL_MESSAGE 
              $INSTALL_CORE 
              $MESSAGE_DONE 
              $KILL_EMACS
             $END"
    cp -r . ~/.emacs.d
    ln -s ~/.emacs.d/emacs-init ~/.emacs
    emacs -q --eval "$INSTALL"
    
elif [ $ACTION == 'sync' ] 
then
    PACKAGE_REFRESH="(package-refresh-contents)"
    SWITCH_BUFFER="(switch-to-buffer \"*Messages*\")"
    MESSAGE_RECOMPILE="(message \"byte-recompiling\")"
    RECOMPILE_PACKAGES="(byte-recompile-directory \"~/.emacs.d\")"
    SYNC_MESSAGE="(message \"Syncing Modules\")"
    SYNC_PACKAGES="(sync-packages)"

    SYNC="$START
          $SWITCH_BUFFER 
          $SYNC_MESSAGE
          $SYNC_PACKAGES
          $MESSAGE_RECOMPILE 
          $RECOMPILE_PACKAGES
          $MESSAGE_DONE
          $KILL_EMACS
         $END"
  emacs --eval "$SYNC"
else
    echo "${ACTION} is not a recognized command."
    echo ""
    echo "Please enter one of \"install\" or \"sync\""
fi
