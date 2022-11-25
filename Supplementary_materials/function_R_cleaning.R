#--------------------------/PREPARATION/--------------------------#

# Function to clean transcription from their diacritics
# The transcriptiosn are within a table
# Use of stringr for the cleaning

clean_data <- function(table){
  
  table %>% 
dplyr::mutate(text_split = stringr::str_extract_all(transcription,"[^\\d|\\W|ˈ|ˌ|̩|͜|‿|̹|̙|
                                      ̃|ʰ|ː|̞|̺|ˀ|ʼ|ˠ|̆|͡|ʳ|̜|̍|
                                      ⁿ|̀|́|̥|̄|̻|̟|̤|̊|ˤ|̼|̘|ˁ|̏|͂|
                                      ʷ|̂|̌|ˑ||̝|̬|̽|ʱ|ǁ|ǀ|̋|
                                      ̯|̠|̪|̰|ʲ|̈|̚]"),
              text_count = stringr::str_count(transcription,"[^\\d|\\W|ˈ|ˌ|̩|͜|‿|̹|̙|
                                      ̃|ʰ|ː|̞|̺|ˀ|ʼ|ˠ|̆|͡|ʳ|̜|̍|
                                      ⁿ|̀|́|̥|̄|̻|̟|̤|̊|ˤ|̼|̘|ˁ|̏|͂|
                                      ʷ|̂|̌|ˑ||̝|̬|̽|ʱ|ǁ|ǀ|̋|
                                      ̯|̠|̪|̰|ʲ|̈|̚]"),
              text_count_full = stringr::str_count(transcription,"."))
}