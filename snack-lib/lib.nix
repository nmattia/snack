{ lib
}: rec {

# All fold functions in this module take a record as follows:
# { f :: elem -> elem'
# , elemLabel :: elem -> label
# , elemChildren :: elem -> [elem]
# }

# foldDAG1 :: Fold -> elem -> elem'
foldDAG1 = fld: root:
  let acc = foldDAGRec fld {} [root];
  in acc.${fld.elemLabel root};

# foldDAG' :: Fold -> [elem] -> [elem']
foldDAG' = fld: roots:
  let acc = foldDAGRec fld {} roots;
  in map (elem: acc.${fld.elemLabel elem}) roots;

# foldDAG :: Fold -> [elem] -> { label -> elem' }
foldDAG = fld: roots:
  foldDAGRec fld {} roots;

# foldDAG' :: Fold -> { label -> elem' } -> [elem] -> { label -> elem' }
foldDAGRec = fld: acc0: roots:
  let
    insert = acc: elem:
      let
        label = fld.elemLabel elem;
        children = fld.elemChildren elem;
      in
        if lib.attrsets.hasAttr label acc
        then acc
        else
          let acc' = acc // { ${label} = fld.f elem; };
          in foldDAGRec fld acc' children;
  in lib.foldl insert acc0 roots;
}
