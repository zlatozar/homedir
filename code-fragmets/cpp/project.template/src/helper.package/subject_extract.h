#ifndef _SUBJECT_EXTRACT_H_
#define _SUBJECT_EXTRACT_H_

#include <string>

namespace helper_package {

  /**
   * @ingroup Helper
   * This is a helper method that is used to find out
   * the text after "Subject:"
   *
   * @param inLine
   *
   * @return the text after "Subject:" if exist
   */
  std::string get_subject_txt(const std::string inLine);

  /**
   * @ingroup Helper
   * @brief This method does something potentially useful.
   *
   * That first sentence will be used as a "one-liner" description,
   * while the rest of the  paragraph will be used in the larger description section.
   *
   * You can have as many paragraphs as you like, separated by spaces.
   *
   * You can even easily make bullet point lists, like:
   *     - item one
   *     - item two
   *     - item three with a really long long long long long long long
   *             description that needs to wrap a line
   *
   * Doxygen will format the above list appropriately.
   *
   * @note Run this program with:  GLOG_logtostderr=1 ./build/project.template
   *
   * <b>You can use HTML in your descriptions as well.</b>
   *
   * @return 0
   */
  int check_input();
}

#endif /* _SUBJECT_EXTRACT_H_ */
