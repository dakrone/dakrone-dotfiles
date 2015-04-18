# for use with `brew install openssh --with-brewed-openssl --with-keychain-support`
# if [ -s /usr/local/bin/ssh-agent ]; then
#     eval $(ssh-agent)

#     function cleanup-ssh-agent {
#         kill -9 $SSH_AGENT_PID
#     }

#     trap cleanup-ssh-agent EXIT
# fi

# brew install keychain
eval $(keychain --eval --agents ssh -Q id_rsa)
