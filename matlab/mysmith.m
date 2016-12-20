

function [B,U,V,Uinv,Vinv] = mysmith(A)
    U = eye(size(A, 1));
    Uinv = eye(size(A, 1));
    V = eye(size(A, 2));
    Vinv = eye(size(A, 2));
    B = A;
    pivot = 1;
    
    while (pivot < size(A,1) && pivot < size(A,2))
        % eliminate rightside
        [C,T,Tinv] = eliminate_rightside(B, pivot);
        B = C;
        V = V*T;
        Vinv = Tinv*Vinv;
        % eliminate downward
        [C,T,Tinv] = eliminate_rightside(transpose(B), pivot);
        B = transpose(C);
        U = transpose(T)*U;
        Uinv = Uinv*transpose(Tinv);
        
        pivot = pivot + 1;
    end
    
end

