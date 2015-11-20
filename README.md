# copybook4java
[![Build Status](https://travis-ci.org/tlbdk/copybook4java.svg?branch=master)](https://travis-ci.org/tlbdk/copybook4java)
[![Coverity Scan Build Status](https://scan.coverity.com/projects/5946/badge.svg)](https://scan.coverity.com/projects/tlbdk-copybook4java)

CopyBook serializer and deserializer for Java where CopyBook lines are used to annotate a normal Java class.

## How to Use It

Annotate request class:

```java
@CopyBook(charset = "cp037")
public class Request {
  @CopyBookLine("02 ID PIC 9(2).")
  private int id;

  @CopyBookLine("02 CMD PIC X(10).")
  private String command;

  @CopyBookLine("01 ARGS OCCURS 10 TIMES.")
  @CopyBookLine("02 ARG PIC X(8).")
  private String[] args;

  public Request(int id, String command, String[] args) {
    this.id = id;
    this.command = command;
    this.args = args;
  }
}
```

Annotate request class:

```java
@CopyBook(charset = "cp037")
public class Response {
  @CopyBookLine("02 ID PIC 9(2).")
  private int id;

  @CopyBookLine("01 LINES OCCURS 80 TIMES.")
  @CopyBookLine("02 LINE PIC X(80).")
  private String[] lines;

  public int getId() {
    return this.id;
  }
  public String[] getLines() {
    return this.lines;
  }
}
```

Construct CopyBookSerializer for Request and Response classes, this will scan the class hierarchy and build a field map that will be used in the serialization and deserialization process:

```java
CopyBookSerializer requestSerializer = new CopyBookSerializer(Request.class);
CopyBookSerializer responseSerializer = new CopyBookSerializer(Response.class);

Request request = new Request(1, "cmd", new String[] { "arg1", "arg2" })
byte[] requestBytes = requestSerializer.serialize(request);

// Call the Backend with whatever client you have for this:
byte[] responseBytes = Backend.submit(requestBytes);

Response response = responseSerializer.deserialize(responseBytes, Response.class);
```

## Supported format annotations and their defaults

```java
@CopyBook(type = FullMapper.class, charset = "UTF-8")
```

The CopyBookFieldFormat annotation can be set on both class level on a individual field to overwrite the defaults:

```java
@CopyBookFieldFormat(type = IntegerToInteger.class, rightPadding = false, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
@CopyBookFieldFormat(type = SignedIntegerToInteger.class, rightPadding = false, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
@CopyBookFieldFormat(type = DecimalToBigDecimal.class, rightPadding = false, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
@CopyBookFieldFormat(type = SignedDecimalToBigDecimal.class, rightPadding = false, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
@CopyBookFieldFormat(type = StringToString.class, rightPadding = true, paddingChar  = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
```

It's also possible to create new CopyBook annotation to give common settings as custom name:

```java
@CopyBook(type = FullMapper.class, charset = "cp037")
@CopyBookFieldFormat(type = StringToString.class, rightPadding = true, paddingChar  = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.LAST_BYTE_EBCDIC_BIT5)
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface DanishIMS { }
```

Using the new annotation:

```java
@DanishIMS
public class Request {
  @CopyBookLine("02 ID PIC 9(2).")
  private int id;
```
