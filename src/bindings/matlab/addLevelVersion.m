function str = addLevelVersion(str, l, v)

if ~isfield(str, 'typecode')
    return;
end;
if length(str) ~= 1 ||strcmp(str.typecode, 'SBML_MODEL') ~= 1
    str = addData(str, l, v);
end;
for i = 1:length(str)
    f = fieldnames(str(i));
    for j = 1:length(f)
        if isstruct(str(i).(f{j}))
            substr = str(i).(f{j});
            str(i).(f{j}) = addLevelVersion(substr, l, v);
        end;
    end;
end;


function str = addData(str, l, v)
f = fieldnames(str);
if sum(ismember(f, 'level')) > 0
    return;
end;
for i=1:length(str)
    str(i).level = l;
    str(i).version = v;
end;