reverse xs = -- linear, tail recursive
        let rev ( [], accum ) = accum
            rev ( y:ys, accum ) = rev ( ys, y:accum )
        in rev ( xs, [] )