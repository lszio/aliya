Write-Output "Hello from Aliya!!!"

function Enable-Proxy() {
    Write-Output "Set proxy to http://localhost:1081"
    Set-Item Env:HTTP_PROXY "http://localhost:1081"
    Set-Item Env:HTTPS_PROXY "http://localhost:1081"
}

function Disable-Proxy() {
    Write-Output "Remove proxy"
    Remove-Item Env:HTTP_PROXY
    Remove-Item Env:HTTPS_PROXY
}

function reload() {
    Write-Output "Reload profile"
    . $profile
}

Set-Alias -Name ep -Value Enable-Proxy
Set-Alias -Name dp -Value Disable-Proxy