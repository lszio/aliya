Write-Output "Hello from Aliya!!!"

function Enable-Proxy() {
    $proxy = "http://localhost:1080"
    Write-Output "Set proxy to $proxy"
    Set-Item Env:HTTP_PROXY $proxy
    Set-Item Env:HTTPS_PROXY $proxy
    git config --global http.proxy $proxy
    git config --global https.proxy $proxy
}

function Disable-Proxy() {
    Write-Output "Remove proxy"
    Remove-Item Env:HTTP_PROXY
    Remove-Item Env:HTTPS_PROXY
    git config --global --unset http.proxy
    git config --global --unset https.proxy
}

function reload() {
    Write-Output "Reload profile"
    . $profile
}

Set-Alias -Name ep -Value Enable-Proxy
Set-Alias -Name dp -Value Disable-Proxy