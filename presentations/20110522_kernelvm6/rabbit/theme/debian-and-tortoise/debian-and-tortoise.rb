# NOTE: This is sumple theme

include_theme("debian-prime")

@image_timer_limit = 8 * 60 # customize
include_theme("image-timer")

@slide_number_uninstall = true
include_theme("slide-number")
include_theme("image-slide-number")

@twitter_stream_content ||='track=#kernelvm,master_q'
include_theme("twitter-footer")
