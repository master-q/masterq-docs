module TwitterUrl (plugin) where
import Network.Gitit.Interface

plugin :: Plugin
plugin = mkPageTransform twitterurlize

twitterurlize :: Inline -> Inline
twitterurlize (Str ('@':x)) =
  Link [Str ('@':x)] ("http://twitter.com/" ++ x, '@':x)
twitterurlize x = x
