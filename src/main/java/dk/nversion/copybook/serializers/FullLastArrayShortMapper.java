/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.serializers;

import dk.nversion.copybook.exceptions.CopyBookException;

public class FullLastArrayShortMapper extends FullMapper {
    @Override
    public void initialize(CopyBookSerializerConfig config) {
        super.initialize(config);
        CopyBookField lastField = this.fields.get(this.fields.size() - 1);
        if (lastField.isArray()) {
            lastField.setMinOccurs(0);
        }
    }
}
