cmake_minimum_required(VERSION 3.12)

configure_file(example1.dat example1.dat COPYONLY)
configure_file(example2.dat example2.dat COPYONLY)
configure_file(example3.dat example3.dat COPYONLY)
configure_file(example4.dat example4.dat COPYONLY)
configure_file(example5.dat example5.dat COPYONLY)

configure_file(example1.out example1.out.expected COPYONLY)
configure_file(example2.out example2.out.expected COPYONLY)
configure_file(example3.out example3.out.expected COPYONLY)
configure_file(example4.out example4.out.expected COPYONLY)
configure_file(example5.out example5.out.expected COPYONLY)

configure_file(run.bat run.bat COPYONLY)

if(WIN32)
    add_test(NAME example1
             COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} $ENV{ComSpec} /c "run.bat" $<TARGET_FILE:SAC> "example1" "Slope-area reach example with level-water surface in cross sections")
    add_test(NAME example2
             COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} $ENV{ComSpec} /c "run.bat" $<TARGET_FILE:SAC> "example2" "Slope-area reach: Snake Creek near Connell, WA for flood of Feb. 21, 1956")
    add_test(NAME example3
             COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} $ENV{ComSpec} /c "run.bat" $<TARGET_FILE:SAC> "example3" "Sloping water surface with conveyance weighting")
    add_test(NAME example4
             COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} $ENV{ComSpec} /c "run.bat" $<TARGET_FILE:SAC> "example4" "Slope-area reach example with level-water surface in cross sections, metric output")
    add_test(NAME example5
             COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} $ENV{ComSpec} /c "run.bat" $<TARGET_FILE:SAC> "example5" "Simple reach example with metric input and feet-second output")
endif()
