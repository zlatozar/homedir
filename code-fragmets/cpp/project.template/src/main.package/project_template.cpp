/**
 * @file   project_template.cpp
 * @author
 * @date
 *
 * @brief  Entry point for the application
 * Configure and runs the application
 *
 * Note that the name is as the name of the application
 */


// _____________________________________________________________________________
//                                                               Includes/Using

#include <iostream>
#include <glog/logging.h>

#include "AppConf.h"
#include "helper.package/include/subject_extract.h"

// _____________________________________________________________________________
//                                                                         RUN

/**
 * Runs the whole application. Only ONE main method has to exist.
 *
 * @param argc not used so we expect compiler to tell us
 * @param argv parameters for GLOG
 *
 * @return success
 */
int main(int argc, char* argv[])
{
  /// or set GLOG_logtostderr=1 in environment
  google::InitGoogleLogging(argv[0]);

  LOG(INFO) << "ver. " << APPLICATION_VERSION_STRING;
  LOG(INFO) << "Start parsing...";
  LOG(INFO) << helper_package::get_subject_txt("Subject: test messages");

  return (0);
}
