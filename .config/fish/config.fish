set PATH $PATH ~/bin
set PATH $PATH /usr/local/sbin
set PATH $PATH /usr/local/bin
set PATH $PATH /usr/local/sbin
set PATH $PATH /usr/libexec
set PATH $PATH /opt/local/bin
set PATH $PATH /opt/local/sbin
set PATH $PATH /usr/local/mysql/bin
set PATH $PATH /usr/local/git/bin
set PATH $PATH /var/lib/gems/1.8/bin

set EDITOR vim
set PAGER less
set TERM xterm-color

set CVSROOT :ext:dakrone@cvsup.rawpacket.org:/home/project/rawpacket/cvs

# Aliases
function lr; ls -lR; end
function l.; ls -d .*; end
function less; less -FRX; end
function ..; cd ..; end
function nsmc; cd ~/src/ruby/nsm-console; end
function serv; cat /etc/services | grep $argv; end
function pg; ps aux | egrep -i --color=auto $argv; end
function ns; sudo netstat -tunapl; end
function dmesg; sudo dmesg; end
function remhex; ssh -i ~/.ssh/id_rawpacket dakrone@localhost -p 6666; end
function remblack; ssh -i ~/.ssh/id_rawpacket hinmanm@localhost -p 7777; end
function scsetup; sudo socat -d -d TCP4-listen:6666,fork OPENSSL:hexbit:443,cert=host.pem,verify=0; end
function scsetup2; sudo socat -d -d TCP4-listen:7777,fork OPENSSL:blackex:443,cert=host.pem,verify=0; end
function blackexprox; ssh -i ~/.ssh/id_rawpacket -ND 9999 hinmanm@localhost -p 7777; end
function blackprox; ssh -i ~/.ssh/id_rawpacket -ND 9999 hinmanm@lblack; end
function zg;
      set d (eval /home/hinmanm/bin/zg.rb $argv)
      cd $d
end

function vless; /usr/share/vim/vim72/macros/less.sh $argv; end

function tcpdump; tcpdump -ttttnnn $argv; end

set fish_greeting ""

function fish_prompt --description 'Write out the prompt'

# Just calculate these once, to save a few cycles when displaying the prompt
      if not set -q __fish_prompt_hostname
            set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
      end

      if not set -q __fish_prompt_normal
            set -g __fish_prompt_normal (set_color normal)
      end

      switch $USER

      case root

      if not set -q __fish_prompt_cwd
            if set -q fish_color_cwd_root
                  set -g __fish_prompt_cwd (set_color $fish_color_cwd_root)
            else
                  set -g __fish_prompt_cwd (set_color $fish_color_cwd)
            end
      end

      printf '%s@%s %s%s%s# ' $USER $__fish_prompt_hostname "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal"

      case '*'

      if not set -q __fish_prompt_cwd
            set -g __fish_prompt_cwd (set_color $fish_color_cwd)
      end

      set blue (set_color blue)
      set cyan (set_color cyan)
      set green (set_color green)
      set red (set_color red)
      set purple (set_color purple)
      set yellow (set_color yellow)

      printf '%s-(%s%s%s@%s%s%s)-%s(%s%s%s)%s> %s' $blue $green $USER $cyan $green $__fish_prompt_hostname $blue $blue $purple (prompt_pwd) $blue $cyan $__fish_prompt_normal

end

end
