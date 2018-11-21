# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
#case "$TERM" in
#xterm-color)
#PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#;;
#*)
#    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#    ;;
#esac

PS1="[\h:\w] "

# Comment in the above and uncomment this below for a color prompt
#PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    #alias ls='ls --color=auto'
    alias ls='ls -asFv --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
fi

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

alias lamarc='ssh lamarc.gs.washington.edu'
alias strackenz='ssh strackenz.spod-central.org'
alias clic1='ssh clic1.gs.washington.edu'
alias phylo='ssh phylo.gs.washington.edu'
alias evo='ssh evolution.gs.washington.edu'

alias rice='ssh teton.bioc.rice.edu'
alias plover='ssh textfire@plover.net'
alias textfire='ssh textfire@plover.net'

alias ll='ls -lv --color=auto'

alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
alias setenv='export'

alias tf='screen /home/lpsmith/bin/tf'
alias rtf='screen -rd'
alias tin='rtin'

alias nedit="gedit"
alias xemacs="LANG=C xemacs"

alias valgrind="valgrind --tool=memcheck --db-attach=yes"

export EDITOR=pico

alias du='du -sk * | sort -rn'

#export PATH="/opt/mono-1.9.1/bin:$PATH":.

#export PKG_CONFIG_PATH="/opt/mono-1.9.1/lib/pkgconfig:$PKG_CONFIG_PATH"

#export MANPATH="/opt/mono-1.9.1/share/man:$MANPATH"

export NNTPSERVER="news.u.washington.edu"

alias mono='LD_LIBARARY_PATH=/opt/mono-1.9.1/lib:$LD_LIBRARY_PATH mono'

export SBW_HOME=/home/lpsmith/SBW-2.7.6
export PATH=$PATH:/home/lpsmith/SBW-2.7.6/bin/:.

export JSIMHOME=/home/lpsmith/JSim/
export PATH=$PATH:/home/lpsmith/JSim/linux_i386/bin/
export JSIMSDK=/usr
export JSIMJRE=/usr
export JSIMSRC=/home/lpsmith/JSim/src/
export PATH=$PATH:/home/lpsmith/JSim/src/build/

export CVSROOT=lamarc.gs.washington.edu:/local/cvs/
export CVS_RSH=ssh

export SBMLTEX=/home/lpsmith/libsbml/tex//
export SBMLBIB=/home/lpsmith/libsbml/bib//
export BIBINPUTS=.:${SBMLBIB}::
export BSTINPUTS=.:${SBMLTEX}::
export TEXINPUTS=.:${SBMLTEX}::
export TEXFONTS=.:${SBMLTEX}::
export MFINPUTS=.:$SBMLTEX}::

alias hg="env PYTHONPATH=/usr/lib/python2.6/site-packages hg"

alias sfweb='sftp luciansmith,antimony@frs.sourceforge.net'
alias sffiles='sftp luciansmith@frs.sourceforge.net'

alias windows="sudo mount -t cifs //128.208.17.109/Users/lpsmith /windows --verbose -o user=lpsmith"
alias g="~/.grepantimony"

alias which="type -a"

alias share="sudo mount -t vboxsf"
