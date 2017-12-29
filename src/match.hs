module Match (
    DocumentId
  , index, query, query'
  ) where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type Document = [Char]
type DocumentId = Int
type Corpus = V.Vector Document
type Cursor = S.Set Int
type Cursors = M.Map DocumentId Cursor
type Edges = M.Map Char Cursors

data Index = Index{
    edges  :: Edges
  , corpus :: Corpus
  }

index :: [Document] -> Index
index corpus =
  let corpus' = zip [0..] corpus in
  let edges = L.foldl
        indexCorpus
        M.empty
        corpus'
  in Index{edges = edges, corpus = V.fromList corpus}
  where mergeCursors :: Cursors -> Cursors -> Cursors
        mergeCursors old new = M.unionWith S.union old new
        indexDoc :: Edges -> DocumentId -> (Int, Char) -> Edges
        indexDoc idx docId (i, c) =
          let defVal = M.singleton docId $ S.singleton i in
          M.insertWith mergeCursors c defVal idx
        indexCorpus :: Edges -> (DocumentId, Document) -> Edges
        indexCorpus idx (id, doc) = L.foldl
          (\idx' (i, c) -> indexDoc idx' id (i, c))
          idx
          (zip [0..] doc)

query :: Index -> [Char] -> Cursors
query idx alphs =
  let cursors = L.foldl
        (\cursors c -> Just $ query' idx c cursors)
        Nothing
        alphs
  in case cursors of
    Nothing       -> M.empty
    Just cursors' -> cursors'

query' :: Index -> Char -> Maybe Cursors -> Cursors
query' idx c cursors = case cursors of
  Nothing       -> results
  Just cursors' -> M.intersectionWith (\old new -> new) results cursors'
  where results = M.findWithDefault M.empty c (edges idx)
