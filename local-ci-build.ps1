$env:PACKAGE_DIR="solver.cap"
$env:ONLINE_UPDATE_SRC_DIR="online_update_v4\dev_v4_src"
$env:ONLINE_UPDATE_SRC_DIR_DEFAULT="online_update_v4\prod_v4_src"
$env:BUILD_TYPE="Release"

##$env:SOURCE_DIR="SOURCE"
$env:BUILD_DIR="BUILD"
$env:INSTALL_DIR="INSTALL"
$env:ONLINE_UPDATE_REPO="i-RIC/online_update_v4"

$env:api_url="https://api.github.com"
$env:IRICLIB_API_URL="$env:api_url/repos/i-RIC/iriclib_v4"
$env:IFW_API_URL="$env:api_url/repos/scharlton2/ifw"

# Download latest iricsdk
$latest = (Invoke-WebRequest -Uri $env:IRICLIB_API_URL/releases/latest -Method GET).Content | ConvertFrom-Json
$url = ($latest.assets | Where-Object {$_.name -match "^iricsdk-(?<major>0|[1-9]\d*)\.(?<minor>[0|1-9]\d*)\.(?<patch>[0|1-9]\d*)\.7z$"}).browser_download_url
if (-not(Test-Path -Path "iricsdk")) {
    curl -LO $url
    $7zfile = $matches[0]
    7z x $7zfile
    Remove-Item $7zfile
}

# export environment variable IRICLIB_VERSION for later steps
$IRICLIB_VERSION="$($matches.major).$($matches.minor).$($matches.patch)"

# Export POCO version
Get-ChildItem .\iricsdk | Where-Object {
    $_.name -match 'poco-(?<version>.*)'
} | Out-Null
$POCO_VER="$($matches.version)"

# Export HDF5 version
Get-ChildItem .\iricsdk | Where-Object {
    $_.name -match 'hdf5-(?<version>.*)'
} | Out-Null
$HDF5_VER="$($matches.version)"

# Configure CMake
$env:workspace="$(pwd)".Replace('\', '/')

$env:CMAKE_PREFIX_PATH="$env:workspace/iricsdk/iriclib-$IRICLIB_VERSION/lib/cmake/iriclib;$env:workspace/iricsdk/hdf5-$HDF5_VER/cmake/hdf5;$env:workspace/iricsdk/poco-$POCO_VER/lib/cmake/Poco"
echo $env:CMAKE_PREFIX_PATH

echo "cmake -S $env:workspace -B $env:workspace/$env:BUILD_DIR -DCMAKE_INSTALL_PREFIX=$env:workspace/$env:INSTALL_DIR"
cmake -S $env:workspace -B $env:workspace/$env:BUILD_DIR -DCMAKE_INSTALL_PREFIX=$env:workspace/$env:INSTALL_DIR

echo "cmake --build $env:workspace/$env:BUILD_DIR --config Debug"
cmake --build $env:workspace/$env:BUILD_DIR --config Debug

echo "ctest --test-dir $env:workspace/$env:BUILD_DIR -C Debug"
ctest --test-dir $env:workspace/$env:BUILD_DIR -C Debug

echo "cmake --build $env:workspace/$env:BUILD_DIR --config Release"
cmake --build $env:workspace/$env:BUILD_DIR --config Release

echo "ctest --test-dir $env:workspace/$env:BUILD_DIR -C Release"
ctest --test-dir $env:workspace/$env:BUILD_DIR -C Release

