package dk.nversion.copybook.serializers;

import dk.nversion.copybook.exceptions.CopyBookException;

public class FullLastArrayShortMapper extends FullMapper {
    @Override
    public void initialize(CopyBookSerializerConfig config) throws CopyBookException {
        super.initialize(config);
        CopyBookField lastField = this.fields.get(this.fields.size() - 1);
        if (lastField.isArray()) {
            lastField.setMinOccurs(0);
        }
    }
}
