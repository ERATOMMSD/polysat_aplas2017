function a = approximate2_help(t, depth)
    if(depth == 0)
        a = t;
        return;
    end
    if(isAlways(sum(t) == 0))
        a = t;
        return;
    end
%     den = min(ab);
%     pivot = index_find(ab, den);
    pivot = index_nonzero_min(t);
    den = t(pivot);
    sim = quorem(t, den);
    

    if(depth == 1)
        sim = sim/gcd(sim);
        a = sim;
        return;
    else
        r = t - sim*den;
        r(pivot) = t(pivot);
        rsim = approximate2_help(r, depth - 1);
%         sim, rsim, r
        a = sim*rsim(pivot) + rsim*sim(pivot);
        a(pivot) = rsim(pivot)*sim(pivot);
        return;
    end
end

