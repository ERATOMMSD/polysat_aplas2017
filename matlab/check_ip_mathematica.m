function b = check_ip_mathematica(ip_cand, form1, form2, vars)
ip_cand = strrep(ip_cand, ' = ', ' == ');
form1 = strrep(form1, ' = ', ' == ');
form2 = strrep(form2, ' = ', ' == ');
fid = fopen('~/mathematica/ips.mathematica', 'w');
fprintf(fid, 'ip = %s;\n', ip_cand);
fprintf(fid, 'form1 = %s;\n', form1);
fprintf(fid, 'form2 = %s;\n', form2);
fprintf(fid, 'b1 = FindInstance[form1 && Not[ip], {%s}];\n', vars);
fprintf(fid, 'b2 = FindInstance[form2 && ip, {%s}];\n', vars);
fprintf(fid, 'res = (b1 == {}) && (b2 == {});\n');
fprintf(fid, 'Export["~/mathematica/out.dat", res];\n');
fclose(fid);
system('/Applications/Mathematica.app/Contents/MacOS/MathematicaScript -script ~/mathematica/ips.mathematica');
d = importdata('~/mathematica/out.dat');
b = strcmp(d, 'True');
% fid = fopen('~/mathematica/ip.dat', 'w')
% fprintf(fid, ip_cand);
% fclose(fid);
% fid = fopen('~/mathematica/form1.dat', 'w')
% fprintf(fid, form1);
% fclose(fid);
% fid = fopen('~/mathematica/form2.dat', 'w')
% fprintf(fid, form2);
% fclose(fid);
% fid = fopen('~/mathematica/vars.dat', 'w');
% fprintf(fid, vars);
% fclose(fid);
% system('~/mathematica/check_ip.mathematica');
% d = importdata('~/mathematica/out.dat');
% b = strcmp(d, 'True');
end