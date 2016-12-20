

function [B,V,Vinv] = eliminate_rightside(A, pivot)
    V = eye(size(A, 2));
    Vinv = eye(size(A, 2));
    B = A;

    % eliminate rightside
    while(true)
        rightside_in = B(pivot, pivot:size(B,2));
        rightside_out = B(pivot, (pivot + 1):size(B,2));
        f = findindex(rightside_out);
        if(f == 0)
            break
        else
            % exchange
            swapi = index_nonzero_min(abs(rightside_in));
            if swapi == 0
                swapi = pivot;
            else
                swapi = pivot + swapi - 1;
            end
            mult = swapcolumn(size(V), pivot, swapi);
            B = B*mult;
            V = V*mult;
            Vinv = swapcolumn(size(V), pivot, swapi)*Vinv;
            % eliminate
            for i = (pivot + 1):size(A, 2)
                m = -(B(pivot, i) - rem(B(pivot, i), B(pivot, pivot)))/B(pivot, pivot);
                mult = addcolumn(size(B, 2), m, pivot, i);
                B = B*mult;
                V = V*mult;
                Vinv = addcolumn(size(B, 2), -m, pivot, i)*Vinv;
            end
        end
    end

    
end

