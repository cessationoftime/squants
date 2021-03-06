/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package squants.motion

import org.scalatest.{ Matchers, FlatSpec }
import squants.space.{CubicMeters, Litres }
import squants.time.Seconds
import squants.{ QuantityParseException, CustomMatchers }

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 */
class VolumeFlowSpec extends FlatSpec with Matchers with CustomMatchers {

  behavior of "VolumeFlow and its Units of Measure"

  it should "create values using UOM factories" in {
    CubicMetersPerSecond(1).toCubicMetersPerSecond should be(1)
	LitresPerSecond(1).toLitresPerSecond should be(1)
	MillilitresPerSecond(1).toMillilitresPerSecond should be(1)
	MicrolitresPerSecond(1).toMicrolitresPerSecond should be(1)
	NanolitresPerSecond(1).toNanolitresPerSecond should be(1)
    CubicFeetPerHour(1).toCubicFeetPerHour should be(1)
    GallonsPerDay(1).toGallonsPerDay should be(1)
    GallonsPerHour(1).toGallonsPerHour should be(1)
    GallonsPerMinute(1).toGallonsPerMinute should be(1)
    GallonsPerSecond(1).toGallonsPerSecond should be(1)
  }

  it should "create values from properly formatted Strings" in {
    VolumeFlow("10.22 m³/s").get should be(CubicMetersPerSecond(10.22))
	VolumeFlow("10.22 l/s").get should be(LitresPerSecond(10.22))
	VolumeFlow("10.22 ml/s").get should be(MillilitresPerSecond(10.22))
	VolumeFlow("10.22 µl/s").get should be(MicrolitresPerSecond(10.22))
	VolumeFlow("10.22 nl/s").get should be(NanolitresPerSecond(10.22))
    VolumeFlow("10.22 ft³/hr").get should be(CubicFeetPerHour(10.22))
    VolumeFlow("10.22 GPD").get should be(GallonsPerDay(10.22))
    VolumeFlow("10.22 GPH").get should be(GallonsPerHour(10.22))
    VolumeFlow("10.22 GPM").get should be(GallonsPerMinute(10.22))
    VolumeFlow("10.22 GPS").get should be(GallonsPerSecond(10.22))
    VolumeFlow("10.22 zz").failed.get should be(QuantityParseException("Unable to parse VolumeFlow", "10.22 zz"))
    VolumeFlow("zz m³/s").failed.get should be(QuantityParseException("Unable to parse VolumeFlow", "zz m³/s"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = CubicMetersPerSecond(10.22)
    x.toCubicMetersPerSecond should be(10.22)
	x.toLitresPerSecond should be(CubicMeters(10.22).toLitres / Seconds(1).toSeconds)
	x.toMillilitresPerSecond should be(CubicMeters(10.22).toMillilitres / Seconds(1).toSeconds)
	x.toMicrolitresPerSecond should be(CubicMeters(10.22).toMicrolitres / Seconds(1).toSeconds)
	x.toNanolitresPerSecond should be(CubicMeters(10.22).toNanolitres / Seconds(1).toSeconds)
    x.toCubicFeetPerHour should be(CubicMeters(10.22).toCubicFeet / Seconds(1).toHours +- 0.00000001)
    x.toGallonsPerDay should be(CubicMeters(10.22).toUsGallons / Seconds(1).toDays +- 0.00000001)
    x.toGallonsPerHour should be(CubicMeters(10.22).toUsGallons / Seconds(1).toHours)
    x.toGallonsPerMinute should be(CubicMeters(10.22).toUsGallons / Seconds(1).toMinutes +- 0.0000000001)
    x.toGallonsPerSecond should be(CubicMeters(10.22).toUsGallons / Seconds(1).toSeconds)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    CubicMetersPerSecond(1).toString(CubicMetersPerSecond) should be("1.0 m³/s")
	LitresPerSecond(1).toString(LitresPerSecond) should be("1.0 l/s")
	MillilitresPerSecond(1).toString(MillilitresPerSecond) should be("1.0 ml/s")
	MicrolitresPerSecond(1).toString(MicrolitresPerSecond) should be("1.0 µl/s")
	NanolitresPerSecond(1).toString(NanolitresPerSecond) should be("1.0 nl/s")
    CubicFeetPerHour(1).toString(CubicFeetPerHour) should be("1.0 ft³/hr")
    GallonsPerDay(1).toString(GallonsPerDay) should be("1.0 GPD")
    GallonsPerHour(1).toString(GallonsPerHour) should be("1.0 GPH")
    GallonsPerMinute(1).toString(GallonsPerMinute) should be("1.0 GPM")
    GallonsPerSecond(1).toString(GallonsPerSecond) should be("1.0 GPS")
  }

  it should "return Volume when multiplied by Time" in {
    CubicMetersPerSecond(1) * Seconds(1) should be(CubicMeters(1))
	LitresPerSecond(1) * Seconds(1) should be(Litres(1))
  }

  behavior of "VolumeFlowConversions"

  it should "provide aliases for single unit values" in {
    import VolumeFlowConversions._

    cubicMeterPerSecond should be(CubicMetersPerSecond(1))
	litrePerSecond should be(LitresPerSecond(1))
	millilitrePerSecond should be(MillilitresPerSecond(1))
	microlitrePerSecond should be(MicrolitresPerSecond(1))
	nanolitrePerSecond should be(NanolitresPerSecond(1))
    cubicFeetPerHour should be(CubicFeetPerHour(1))
    gallonPerDay should be(GallonsPerDay(1))
    gallonPerHour should be(GallonsPerHour(1))
    gallonPerMinute should be(GallonsPerMinute(1))
    gallonPerSecond should be(GallonsPerSecond(1))
  }

  it should "provide implicit conversion from Double" in {
    import VolumeFlowConversions._

    val d = 10.22d
    d.cubicMetersPerSecond should be(CubicMetersPerSecond(d))
	d.litresPerSecond should be(LitresPerSecond(d))
	d.millilitresPerSecond should be(MillilitresPerSecond(d))
	d.microlitresPerSecond should be(MicrolitresPerSecond(d))
	d.nanolitresPerSecond should be(NanolitresPerSecond(d))
    d.cubicFeetPerHour should be(CubicFeetPerHour(d))
    d.gallonsPerDay should be(GallonsPerDay(d))
    d.gallonsPerHour should be(GallonsPerHour(d))
    d.gallonsPerMinute should be(GallonsPerMinute(d))
    d.gallonsPerSecond should be(GallonsPerSecond(d))
  }

  it should "provide Numeric support" in {
    import VolumeFlowConversions.VolumeFlowNumeric
    implicit val tolerance = GallonsPerDay(0.0000000000001)
    val vfrs = List(GallonsPerDay(24), GallonsPerHour(1))
    vfrs.sum should beApproximately(GallonsPerDay(48))
  }
}
