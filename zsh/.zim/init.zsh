zimfw() { source /home/fakecrafter/.zim/zimfw.zsh "${@}" }
fpath=(/home/fakecrafter/.zim/modules/git/functions /home/fakecrafter/.zim/modules/utility/functions /home/fakecrafter/.zim/modules/duration-info/functions /home/fakecrafter/.zim/modules/git-info/functions ${fpath})
autoload -Uz git-alias-lookup git-branch-current git-branch-delete-interactive git-dir git-ignore-add git-root git-stash-clear-interactive git-stash-recover git-submodule-move git-submodule-remove mkcd mkpw duration-info-precmd duration-info-preexec coalesce git-action git-info
source /home/fakecrafter/.zim/modules/environment/init.zsh
source /home/fakecrafter/.zim/modules/git/init.zsh
source /home/fakecrafter/.zim/modules/input/init.zsh
source /home/fakecrafter/.zim/modules/termtitle/init.zsh
source /home/fakecrafter/.zim/modules/utility/init.zsh
source /home/fakecrafter/.zim/modules/duration-info/init.zsh
source /home/fakecrafter/.zim/modules/asciiship/asciiship.zsh-theme
source /home/fakecrafter/.zim/modules/zsh-completions/zsh-completions.plugin.zsh
source /home/fakecrafter/.zim/modules/completion/init.zsh
source /home/fakecrafter/.zim/modules/zsh-autosuggestions/zsh-autosuggestions.zsh
source /home/fakecrafter/.zim/modules/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /home/fakecrafter/.zim/modules/zsh-history-substring-search/zsh-history-substring-search.zsh
source /home/fakecrafter/.zim/modules/spaceship/spaceship.zsh