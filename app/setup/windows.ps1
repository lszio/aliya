function check() {
    Write-Output "Checking..."
    if ($ENV:USERNAME -eq 'liszt') {
        $ISME = $true
    }
    else {
        $ISME = $false
    }

    $ENV:HOME = "C:\User\$ENV:USERNAME"
    if ($ISME) {
        $ENV:SCOOP = 'C:\Liszt\Scoop'
        $ENV:ALIYA = "C:\Liszt\Aliya"
    }
    else {
        $ENV:SCOOP = "$ENV:HOME\Scoop"
        $ENV:ALIYA = "$ENV:HOME\Aliya"
    }
    [environment]::setEnvironmentVariable('HOME', $ENV:HOME, 'User')
    [environment]::setEnvironmentVariable('SCOOP', $ENV:SCOOP, 'User')
    [environment]::SetEnvironmentVariable("ALIYA", $ENV:ALIYA, "User")
}

function main() {
    Write-Output "Hello $ENV:USERNAME"
    check
    install
}

function install() {
    Write-Output "Installing..."
    Invoke-Expression (new-object net.webclient).downloadstring('https://get.scoop.sh')

    scoop install git
    scoop bucket add extras

    if ($ISME) {
        scoop install sudo ln
    }
}

function uninstall() {
    Write-Output "Uninstalling..."
}

main