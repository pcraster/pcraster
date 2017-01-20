"""
Utilities for string handling.
"""

import re


def replaceCase(
         caseTypes,
         str,
         wordToReplace,
         replaceWithWord):
  """
  Substitutes a word with different casings.

  @param caseTypes: List of chars: c(orrect case) u(pper case) l(ower case).
  @param str: String to change.
  @param wordToReplace: Word in str to replace.
  @param replaceWithWord: Word to replace wordToReplace with.
  @return: Copy of input string with words replaced.
  """
  result = str;

  for i in caseTypes.lower():
    if i == "c":
      result = result.replace(wordToReplace, replaceWithWord)
    elif i == "u":
      result = result.replace(wordToReplace.upper(), replaceWithWord.upper())
    elif i == "l":
      result = result.replace(wordToReplace.lower(), replaceWithWord.lower())
    else:
      assert i in "ulc"

  return result


# Match program identifiers [a-zA-Z_][a-zA-Z0-9_]* listed in replaceDict as
# key and replace with their dictionary value.
# str = 'change this but not_this'
# replaceDict = { 'this':'that' }
# returns 'change that but not_this'
def replaceIdentifiers( str, replaceDict):

  def replaceId(matchObj):
     match = matchObj.string[matchObj.start():matchObj.end()]
     if match in replaceDict:
       return replaceDict[match]
     return match

  return re.sub(r'([a-zA-Z_][a-zA-Z_0-9]*)', replaceId, str)
