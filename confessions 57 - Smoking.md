![header](https://filebox.tymoon.eu/file/TmpRNQ==) After a long time of frustration, trial, and mostly error, [qt-libs](https://github.com/Shinmera/qt-libs) finally supports Windows. In this entry I'll do something that will most likely not be of interest to many people, but I still think it is worth recording for posterity. Specifically, I will describe the way to build the smoke libraries on Windows. As with anything that combines the terms "C++", "building", and "Windows", I cannot guarantee that it will work straight away. Regardless, I hope that it provides enough of a framework to guide you most of the way, if you ever do find yourself in this unfortunate position.

The first thing that you need is a copy of Visual Studio 2012. I don't know if the express edition or other versions work as well, as I haven't bothered to test them. I expect at least the express edition should work. Once it is installed, make sure to install all required Windows Updates, as they will most likely fix a problem with VS. Also make sure that your system locale is English, or something else in the `latin-1` range, or the build will likely error in mysterious ways.

Next, you'll need to download a bunch of stuff and extract it to `C:\Qt`:

* <https://github.com/Shinmera/qt4/archive/qt-libs1.1.0.zip>  
  to `C:\Qt\4.8.7`
* <https://github.com/Shinmera/smokegen/archive/qt-libs1.1.0.zip>  
  to `C:\Qt\smokegen`
* <https://github.com/Shinmera/smokeqt/archive/qt-libs1.1.0.zip>  
  to `C:\Qt\smokeqt`
* <https://github.com/Shinmera/libcommonqt/archive/master.zip>  
  to `C:\Qt\libcommonqt`
* <http://download.qt.io/official_releases/jom/jom.zip>  
  to `C:\Qt\jom`
* <http://www.cmake.org/files/v3.3/cmake-3.3.0-rc2-win32-x86.exe>  
  simply install it.

I'm linking to my own repositories for the most part, as I had to commit one or two patches on some of these. The tags also ensure that you're downloading a version that I tested successfully. Now you need to configure your environment variables. Add a variable called `QTDIR` with the value `C:\Qt\4.8.7`. Extend your `PATH` with `%QTDIR%\bin`. Next, open up a Visual Studio *native* command line, either for `x64` or `x86`, depending on which target you want to build for. Time to do some building!

    cd %QTDIR%
    configure -opensource -release -platform win32-msvc2012 -fast -nomake docs -nomake demos -nomake examples

If configure complains about anything, figure out how to satisfy its dependencies. Otherwise, this configures Qt in release mode and tries to set up as fast a build as possible without deactivating any of the actual Qt features. Once `configure` is done, it's time to launch the build.

    ..\jom\jom.exe -j 6

Substitute `6` for however many cores you have available. This will take *a while*. Depending on how fast your machine is and how many cores you dedicate to it, it can take a couple of hours. Get ready to do something else productive during that time.

At some point the build will almost certainly error, mentioning something about a missing WebKit library file. This is a bug in Qt's build system on windows, but it is luckily easily fixed. Go to `C:\Qt\4.8.7\lib` and copy `QtWebKit4.lib` to `QtWebKit.lib`. I'm not entirely sure anymore if that's the exact file name, but I *am* sure that you can figure out the proper names to use from the last error messages on your command line. Once you copied the file to the proper name it expects, simply re-run the previous `jom` command and it should push through.

Qt should build just fine from that. Once it is done, it's time to get to the more finicky parts of this process, namely smoke generator.

    cd C:\Qt\smokegen
    cmake . -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="C:/Qt/smokegen-install" -DQT_QMAKE_EXECUTABLE="C:/Qt/4.8.7/bin/qmake.exe"
    ..\jom\jom.exe -j 6

Supplying the `qmake` executable explicitly is not always necessary. However, it is the safe way to go, especially if you happen to already have a different Qt installation on your system. Fortunately, building the smoke generator should not take long.

    nmake install

Once it's done installing, we can move on to the even more finicky part, the actual smoke Qt bindings.

    cd C:\Qt\smokeqt
    cmake . -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="C:/Qt/smokeqt-install" -DQT_QMAKE_EXECUTABLE="C:/Qt/5.8.7/bin/qmake.exe" -DSmoke_DIR="C:/Qt/smokegen-install/share/smoke/cmake"
    ..\jom\jom.exe -j 6

Again, we're supplying explicit paths, because I don't trust cmake to be smart enough to figure out exactly what we want. Building these will take a while longer, and it's also the most likely step to fail in some fashion.

If you encounter some weird error that complains about `echo` just when it tries to generate the bindings for QtCore, the most likely suspect is that you have binaries somewhere in your `PATH` that conflict with the build system. Adjust your `PATH` to only contain the directories you absolutely need.

Another error that is likely to occur is that it'll fail to build `QtTest`. I don't know why this is, and I wasn't able to rake up the energy to try and fix it. For now, you can comment out line `63` in `CmakeLists.txt`, re-run the `cmake` call, and then start the build process again. That will simply skip `QtTest`.

Other errors can sometimes be fixed by rebooting your system, or starting up a new Visual Studio command line and trying again. For anything else, you are unfortunately on your own to figure it out. Good luck!

Once it is done building, issue the install command as before.

    nmake install

Finally, the last step is to build `libcommonqt` itself.

    cd C:\Qt\libcommonqt
    qmake

This will create a Visual Studio project file. Launch Visual Studio, furiously close its 'welcome screen', and open the project file. Switch the build type to `Release` and make sure the proper platform is selected. Right click on the project in the solution explorer and open `Properties`. Switch to `VC++ Directories`. Edit `Include Directories` and prepend

    C:\Qt\smokegen-install\include;C:\Qt\smokeqt-install\include;

Similarly edit `Library Directories` to prepend

    C:\Qt\smokegen-install\lib;C:\Qt\smokeqt-install\lib;

Confirm the dialog with `Ok`. Finally, build the library by right clicking the project again and selecting `Build`.

This should only take a few seconds. Once it's done, the generated DLL will be in `C:\Qt\libcommonqt\release\commonqt.dll`. If you've come this far, congratulations! You managed to build all the necessary libraries to run CommonQt.

If you want to use qt-libs, open up its source folder, create a sub folder called `standalone`, and copy all DLLs from `C:\Qt\4.8.7\bin`, `C:\Qt\smokegen-install\bin`, `C:\Qt\smokeqt-install\bin`, and `C:\Qt\libcommonqt\release\` into it. That should ensure that it'll be able to find and set them up properly on its own. Getting CommonQt set up is then only a matter of `(ql:quickload :qt-libs)`.

For most of you, this whole charade won't be necessary. Simply quickloading `qt-libs` --at least, any version newer than 2015.06.25-- should download usable libraries automatically for you. You can also download them manually from [the](https://github.com/Shinmera/qt4/releases/tag/qt-libs1.1.0) [Git](https://github.com/Shinmera/smokegen/releases/tag/qt-libs1.1.0) [Hub](https://github.com/Shinmera/smokeqt/releases/tag/qt-libs1.1.0) [releases](https://github.com/Shinmera/libcommonqt/releases/tag/qt-libs1.1.0).

If you still need to build them yourself and follow this guide, you will be mostly on your own. I'm not interested in providing any support for building this beyond what this blog entry offers. Just getting to this point alone has been painful enough for me to not want to have anything further to do with it. Either way, good luck to you.

![footer](https://filebox.tymoon.eu/file/TmpVdw==)
