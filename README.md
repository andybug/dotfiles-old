# dotfiles
Use stow.

## weechat
Add login for each server like so:

    /secure passphrase xxxxxx
    /secure set freenode_password xxxxxxx

Which is referenced in `irc.conf` as:

    freenode.sasl_password = "${sec.data.freenode_password}"

