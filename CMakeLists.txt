cmake_minimum_required(VERSION 3.5)
project(rclada_fosdem20 VERSION 0.1.0)

find_package(rclada_common REQUIRED)
ada_begin_package()

find_package(geometry_msgs REQUIRED)
find_package(rclada REQUIRED)
find_package(rosidl_generator_ada REQUIRED)

ada_import_c_libraries(${geometry_msgs_LIBRARIES})
ada_import_msgs(geometry_msgs)

ada_add_executables(
        fosdem20.gpr
        ${PROJECT_SOURCE_DIR} bin

        turtlesim_command)

ada_end_package()
