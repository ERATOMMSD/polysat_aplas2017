function t = cut_epsilon(t, up)
    m = max(abs(t));
    for i=1:length(t)
        if isAlways(abs(t(i)) < m/up)
            t(i) = 0;
        end
    end
end

