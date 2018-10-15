import objsets.GoogleVsApple.google
import objsets.TweetReader.allTweets
import objsets._

val googleTweets: TweetSet = allTweets.filter(tweet => tweet.text.contains('a'))