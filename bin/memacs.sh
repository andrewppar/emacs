ACTION=$1
if [ $ACTION == 'install' ]
then
  emacs -q --eval "(progn (insert \"Loading Core...\") (load  \"~/.emacs.d/core.el\") (insert \"Updating Packages\") (install-core) (insert \"Done!\") (kill-emacs))"
elif [ $ACTION == 'sync' ] 
then
  emacs  --eval "(progn (insert \"Syncing Modules\") (sync-packages) (insert \"Done!\") (kill-emacs))"
else
  echo "${ACTION} is not a recognized command"
fi
