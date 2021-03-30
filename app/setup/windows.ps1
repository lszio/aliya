function check() {
    Write-Output "Checking..."

    if (!$ENV:HOME) {
        $ENV:HOME = "C:\Users\$ENV:USERNAME" # 默认盘符
        [environment]::setEnvironmentVariable('HOME', $ENV:HOME, 'User')
    }
    
    if (!$ENV:SCOOP) {
        $ENV:SCOOP = If ($ISME) { 'C:\Liszt\Scoop' } Else { "$ENV:HOME\Scoop" }
        [environment]::setEnvironmentVariable('SCOOP', $ENV:SCOOP, 'User')
    }
    
    if (!$ENV:ALIYA) {
        $ENV:ALIYA = If ($ISME) { 'C:\Liszt\Aliya' } Else { "$ENV:HOME\Aliya" }
        [environment]::SetEnvironmentVariable("ALIYA", $ENV:ALIYA, "User")
    }
}

function install() {
    Write-Output "Installing..."
    if (!$INSTALLED) {
        Invoke-Expression (new-object net.webclient).downloadstring('https://get.scoop.sh')
        scoop update
        scoop install git
        scoop bucket add extras
        scoop bucket add java    
        if ($ISME) {
            scoop install sudo ln v2ray v2rayn emacs firefox roswell
        }
    }

    if (!(Test-Path $ENV:ALIYA)){
        echo "Cloning aliya..."
        git clone http://github.com/Liszt21/Aliya $ENV:ALIYA
    }


}

function uninstall() {
    Write-Output "Uninstalling..."
    if (!$INSTALLED) {
        Write-Output "Scoop is not installed"
        exit
    }
}

function Test-Command($command) {
    $ErrorActionPreference = 'stop'
    try {
        Get-Command $command | Out-Null
        return $true
    }
    catch {
        Write-Output "Errorrr"
        return $false
    }
}

function Ping($url) {
    $ErrorActionPreference = 'stop'
    try {
        Test-Connection -Count 1 $url | Out-Null
        return $true
    }
    catch {
        Write-Output "$url not available"
        return $false
    }
}


$ISME = ($ENV:USERNAME -eq 'liszt')
$INSTALLED = (Test-Command scoop)

Write-Output "Hello $ENV:USERNAME"
check
if ($args[0] -eq "uninstall"){
    uninstall
    exit
}
install