/**
  * Created by chris on 2/13/16.
  */



sealed trait Field

final case class UnknownField(name: String, command: String) extends Field
final case class ByteField(name: String) extends Field
final case class IntField(name: String) extends Field
final case class ReferenceField(name: String) extends Field

final case class UnsignedByteField(name: String) extends Field
final case class UnsignedShortField(name: String) extends Field
final case class ShortField(name: String) extends Field
final case class StringArrayField(name: String) extends Field
final case class LongField(name: String) extends Field
final case class QFloatField(name: String) extends Field
final case class AddIntegerField(name: String) extends Field
final case class ByteArrayField(name: String) extends Field
final case class BinfileField(name: String) extends Field
final case class AddCurrentTimestampSecField(name: String) extends Field
final case class AddCurrentTimestampField(name: String) extends Field




object Model {

    def createField(name: String, keys: Map[String, String]): Field = {
        keys("read_command") match {
            case "readByte" => ByteField(name)
            case "readInt" => IntField(name)
            case "readShort" => ShortField(name)

            case "readUnsignedByte" => UnsignedByteField(name)
            case "readUnsignedShort" => UnsignedShortField(name)
            case "reference" => ReferenceField(name)
            case "readStringArray" => StringArrayField(name)
            case "readLong" => LongField(name)
            case "readQfloat" => QFloatField(name)
            case "addInteger" => AddIntegerField(name)
            case "readByteArray" => ByteArrayField(name)
            case "readBinfile" => BinfileField(name)
            case "addCurrentTimestampSec" => AddCurrentTimestampSecField(name)
            case "addCurrentTimestamp" => AddCurrentTimestampField(name)

            case _ => UnknownField(name, keys("read_command"))
        }
    }

}