function [valid, message] = applyUserValidation(SBMLStructure, level, version, packages, pkgVersion)

valid = 1;
message = '';

%%%%
% example check that all GeneProducts listed have an id value

[present, index] = ismember('fbc', packages);
if (present)
    if (pkgVersion(index) == 2)
        gp = SBMLStructure.fbc_geneProduct;
        [a, num] = size(gp);
        for i=1:num
            if (isempty(gp(num).fbc_id))
                valid = 0;
                message = 'geneProduct is missing the id attribute';
            end;
        end;
    end;
end;