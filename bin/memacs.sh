ACTION=$1
if [ $ACTION == 'install' ]
then
  emacs -q --eval "(progn (insert \"Loading Core...\") (load  \"~/.emacs.d/core.el\") (insert \"Updating Packages\") (install-core) (insert \"Done!\") (kill-emacs))"
elif [ $ACTION == 'sync' ] 
then
  emacs  --eval "(progn (package-refresh-contents) (insert \"Syncing Modules\") (sync-packages) (insert \"Recompiling...\") (byte-recompile-directory (expand-file-name \"~/.emacs.d\") 0) (insert \"Done!\") (kill-emacs))"
else
  echo "${ACTION} is not a recognized command"
fi
