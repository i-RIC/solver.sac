export PACKAGE_DIR=solver.cap
export ONLINE_UPDATE_SRC_DIR="online_update_v4/dev_v4_src"
export ONLINE_UPDATE_SRC_DIR_DEFAULT="online_update_v4/prod_v4_src"
export BUILD_TYPE="Release"

export BUILD_DIR="BUILD"
export INSTALL_DIR="INSTALL"
export ONLINE_UPDATE_REPO="i-RIC/online_update_v4"

export api_url=https://api.github.com
##export IRICLIB_API_URL=$api_url/repos/i-RIC/iriclib_v4
export IRICLIB_API_URL=$api_url/repos/scharlton2/iriclib_v4
export IFW_API_URL=$api_url/repos/scharlton2/ifw

# Download latest iricsdk
# curl -s https://api.github.com/repos/i-RIC/iriclib_v4/releases/latest | jq -r '.assets[] | select(.name | match("iricsdk-([0-9].*)-ubuntu-22.04.7z")) | .browser_download_url'
url=$(curl -s $IRICLIB_API_URL/releases/latest | jq -r '.assets[] | select(.name | match("iricsdk-([0-9].*)-ubuntu-22.04.7z")) | .browser_download_url')
echo $url
export IRICLIB_VERSION=$(echo $url | sed 's|.*/iricsdk-\([^\.]*\).\([^\.]*\).\([^\.]*\)-ubuntu-22.04.7z|\1.\2.\3|')
if ! [ -d "iricsdk/iriclib-$IRICLIB_VERSION" ]; then
    rm -rf iricsdk
    ##curl -LO $url
    file=$(echo $url | sed 's|.*/iricsdk-\([^\.]*\).\([^\.]*\).\([^\.]*\)-ubuntu-22.04.7z|iricsdk-\1.\2.\3-ubuntu-22.04.7z|')
    echo 7z x $file
    7z x $file
fi
###exit -99
###export IRICLIB_VERSION=4.0.47

# Export POCO version
pocodir=$(find ./iricsdk -maxdepth 1 -type d -regex './iricsdk/poco-\(.*\)')
if [[ "$pocodir" =~ poco-(.*) ]]; then
    POCO_VER="${BASH_REMATCH[1]}"
else
    POCO_VER=""
fi
export POCO_VER
#echo $POCO_VER

# Export HDF5 version
hdf5dir=$(find ./iricsdk -maxdepth 1 -type d -regex './iricsdk/hdf5-\(.*\)')
if [[ "$hdf5dir" =~ hdf5-(.*) ]]; then
    HDF5_VER="${BASH_REMATCH[1]}"
else
    HDF5_VER=""
fi
export HDF5_VER
#echo $HDF5_VER

workspace="$(cd "$(dirname "$0")" && pwd)"
#echo $workspace

# set CMAKE_PREFIX_PATH
export CMAKE_PREFIX_PATH=$workspace/iricsdk/iriclib-$IRICLIB_VERSION/lib/cmake/iriclib:$workspace/iricsdk/hdf5-$HDF5_VER/share/cmake/hdf5:$workspace/iricsdk/poco-$POCO_VER/lib/cmake/Poco
echo $CMAKE_PREFIX_PATH
##exit -99

# configure cmake
echo "cmake -G 'Ninja Multi-Config' -S $workspace -B $workspace/$BUILD_DIR -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DCMAKE_INSTALL_PREFIX=$workspace/$INSTALL_DIR"
cmake -G 'Ninja Multi-Config' -S $workspace -B $workspace/$BUILD_DIR -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DCMAKE_INSTALL_PREFIX=$workspace/$INSTALL_DIR

# build cmake Release
echo "cmake --build $workspace/$BUILD_DIR --config Release"
cmake  --build $workspace/$BUILD_DIR --config Release

# build cmake Debug
echo "cmake --build $workspace/$BUILD_DIR --config Debug"
cmake  --build $workspace/$BUILD_DIR --config Debug

# ctest
echo "ctest --test-dir $workspace/$BUILD_DIR -C Debug"
ctest --test-dir $workspace/$BUILD_DIR -C Debug

# ctest
echo "ctest --test-dir $workspace/$BUILD_DIR -C Debug"
ctest --test-dir $workspace/$BUILD_DIR -C Debug

