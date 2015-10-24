package dk.nversion.copybook.serializers;


public class FullLastArrayShort extends FullSerializer {

    public FullLastArrayShort(CopyBookSerializerConfig config) {
        super(config);
        CopyBookField lastField = this.fields.get(this.fields.size() - 1);
        if (lastField.isArray()) {
            lastField.setMinOccurs(0);
        }
    }
}
