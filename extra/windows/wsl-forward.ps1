$wsladdr  = bash -c "ip -o -4 a s eth0 | sed 's#.*inet ##; s#/.*##'"
$wslports = Read-Host "Ports of your servers inside WSL (space-delimited list)"

-split $wslports | ForEach {

    $wslport = $_;

    echo ""

    $hostaddr = "0.0.0.0"
    $hostport = $wslport

    Write-Host -f magenta "netsh interface portproxy delete v4tov4 listenport=$hostport listenaddress=$hostaddr"
    netsh interface portproxy delete v4tov4 listenport=$hostport listenaddress=$hostaddr

    Write-Host -f magenta "netsh interface portproxy add v4tov4 listenport=$hostport listenaddress=$hostaddr connectport=$wslport connectaddress=$wsladdr"
    netsh interface portproxy add v4tov4 listenport=$hostport listenaddress=$hostaddr connectport=$wslport connectaddress=$wsladdr

    echo "Your server inside WSL at port $wslport can be accessed from Windows at ${hostaddr}:$hostport"

}

echo ""
echo "Done."

Read-Host
