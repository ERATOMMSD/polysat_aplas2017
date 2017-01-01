function a = approximate2(t, depth)
    a = approximate2_help(abs(t), depth);
    for i=1:length(t)
        if isAlways(t(i) < 0)
            a(i) = -a(i);
        end
    end
end

