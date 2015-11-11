library(rmongodb)
library(plyr)
CreateMongoObject <- function(mongo, db) {
  return (list(mongo = mongo, db = db))
}
# Connect to mongo database
# Ex. myMongo <- CreateMongoConnection("mymongo")  # connect to mymongo database
CreateMongoConnection <- function(db) {
  mongo <- mongo.create(host = "localhost")
  mongoObject <- CreateMongoObject(mongo, db)
  return (mongoObject)
}
# Destroy Connection after finish
# Ex. DestroyMongoConnection(myMongo)
DestroyMongoConnection <- function(mongoObject) {
  mongo.destroy(mongoObject$mongo)
}
# Delete collection in mongo database
# Ex. DeleteMongoCollection(myMongo, "mycollection")  # delete "mycollection" collection from "mymongo" database
DeleteMongoCollection <- function(mongoObject, collection) {
  mongo.drop(mongoObject$mongo, paste0(mongoObject$db, ".", collection))
}
# Load whole mongo collection as dataframe
# Ex. LoadDataframeFromMongo(myMongo, "mycollection")  # load all "mycollection" data into a dataframe
LoadDataframeFromMongo <- function(mongoObject, collection) {
  cursor <- mongo.find(mongoObject$mongo, paste0(mongoObject$db, ".", collection))
  data.df <- MyCursorToDataframe(cursor)
  return (data.df)
}
# Change rmongodb's cursor to a dataframe
# Ex. see function LoadDataframeFromMongo
MyCursorToDataframe <- function (cursor, nullToNA = TRUE) 
{
    res <- data.frame()
    while (mongo.cursor.next(cursor)) {
        val <- mongo.bson.to.list(mongo.cursor.value(cursor))
        if (nullToNA  ==  TRUE) 
            val[sapply(val, is.null)] <- NA
        val <- val[sapply(val, class) !=  "mongo.oid"]
        res <- rbind.fill(res, as.data.frame(val, stringsAsFactors = F))
    }
    return (res)
}
# Save Dataframe to mongo collection
# Ex. SaveDFToMongo(myMongo, "mycollection", my.data.df)
SaveDFToMongo <- function(mongoObject, collection, data.df) {
  library(jsonlite)
  dbns <- paste0(mongoObject$db, ".", collection)
  if (mongo.is.connected(mongoObject$mongo) == T) {
    lst <- split(data.df, rownames(data.df))
    bson.lst <- lapply(lst, mongo.bson.from.list)
    mongo.insert.batch(mongo = mongoObject$mongo, ns = dbns, lst = bson.lst)
  } else {
    print("mongo loss connection")
  }
}
# Check if collection exist in the database
IsCollectionExist <- function(mongoObject, collection) {
  dbns <- paste0(mongoObject$db, ".", collection)
  return (!is.null(mongo.find.one(mongoObject$mongo, dbns)))
}