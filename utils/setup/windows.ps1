function check() {
    Write-Output "Checking..."

    if (!$ENV:HOME) {
        $ENV:HOME = "C:\Users\$ENV:USERNAME" # 默认盘符
        [environment]::setEnvironmentVariable('HOME', $ENV:HOME, 'User')
    }
    
    if (!$ENV:SCOOP) {
        $ENV:SCOOP = "$ENV:HOME\Scoop"
        [environment]::setEnvironmentVariable('SCOOP', $ENV:SCOOP, 'User')
    }
    
    if (!$ENV:ALIYA) {
        $ENV:ALIYA = "$ENV:HOME\Aliya"
        [environment]::SetEnvironmentVariable("ALIYA", $ENV:ALIYA, "User")
    }
}

function install() {
    Write-Output "Installing..."
    if (!$INSTALLED) {
        Invoke-Expression (new-object net.webclient).downloadstring('https://get.scoop.sh')
        scoop update
        scoop install git sudo ln lxrunoffline fd ripgrep curl make
        scoop bucket add extras
        scoop bucket add java  
        scoop bucket add nerd-fonts  
        if ($ISME) {
            scoop install v2ray v2rayn emacs atom vim vscode yarn pyenv nvm roswell julia openjdk
            scoop install googlechrome firefox sumatrapdf potplayer vcxsrv snipaste
            scoop install firecode sarasagothic-sc 
        }
    }


    if (!(Test-Path $ENV:ALIYA)) {
        Write-Output "Cloning aliya..."
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
        return $false
    }
}

function Set-Profile() {
    try {
        $content = Get-Content $profile
        if ($content -match "\. .*\\etc\\profile.ps1") {
            Write-Output "profile already included"
        }
        else {
            $content = "$content`n. $env:aliya\etc\profile.ps1"
        }
    }
    catch {
        $content = ". " + $profile
    }
    finally {
        Write-Output "Write '$content' to $profile "
        Set-Content $profile $content
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
if ($args[0] -eq "uninstall") {
    uninstall
    exit
}
install
