{ lib
}: rec {

# Like foldDAG' but with a single root
foldDAG1 = f: elemLabel: elemChildren: root:
  let
    acc = foldDAGRec f elemLabel elemChildren {} [root];
  in acc.${elemLabel root};

# Like foldDAG but returns the updated roots instead of the accumulator
foldDAG' = f: elemLabel: elemChildren: roots:
  let
    acc = foldDAGRec f elemLabel elemChildren {} roots;
  in map (elem: acc.${elemLabel elem}) roots;

foldDAG = f: elemLabel: elemChildren: roots:
  foldDAGRec f elemLabel elemChildren {} roots;

foldDAGRec = f: elemLabel: elemChildren: acc0: roots:
  let
    insert = acc: elem:
      let
        label = elemLabel elem;
        children = elemChildren elem;
      in
        if lib.attrsets.hasAttr label acc
        then acc
        else
          let acc' = acc // { ${label} = f elem; };
          in foldDAGRec f elemLabel elemChildren acc' children;
  in lib.foldl insert acc0 roots;

}
