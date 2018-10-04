package cz.tconsult.lib.softwareinfo.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import cz.tconsult.lib.softwareinfo.SoftwareInfo;
import cz.tconsult.lib.softwareinfo.SoftwareInfoBean;
import cz.tconsult.lib.softwareinfo.SoftwareInfoPair;

/**
 */
public class SoftwareInfoPairTest {

  @Test
  public void testRightVersion1() throws Exception {
    final SoftwareInfoPair sip = createSoftwareInfoPair("CIBIS-Web", "v2017S-02.09.00-27-g186aacc", "CIBIS-Web", "v2017S-02.09.00-23-gcea3a39");

    assertTrue(sip.isRightVersion());
  }

  @Test
  public void testRightVersion2() throws Exception {
    final SoftwareInfoPair sip = createSoftwareInfoPair("CIBIS-Web", "UnspecifiedB", "CIBIS-Web", "UnspecifiedB");

    assertFalse(sip.isRightVersion());
  }

  @Test
  public void testRightVersion3() throws Exception {
    final SoftwareInfoPair sip = createSoftwareInfoPair("CIBIS-Web", "v2017S-02.09.00", "CIBIS-Web", "v2017S-02.09.00");

    assertTrue(sip.isRightVersion());
  }

  @Test
  public void testRightVersion4() throws Exception {
    final SoftwareInfoPair sip = createSoftwareInfoPair("CIBIS-Web", "v2017S-02.08.00", "CIBIS-Web", "v2017S-02.09.00");

    assertFalse(sip.isRightVersion());
  }

  @Test
  public void testRightVersion5() throws Exception {
    final SoftwareInfoPair sip = createSoftwareInfoPair("CIBIS-Web", "v2017S", "CIBIS-Web", "v2017S");

    assertTrue(sip.isRightVersion());
  }

  private SoftwareInfoPair createSoftwareInfoPair(final String programName1, final String implementationVersion1, final String programName2, final String implementationVersion2) {
    final SoftwareInfoBean sib1 = new SoftwareInfoBean();
    sib1.setProgramName(programName1);
    sib1.setImplementationVersion(implementationVersion1);
    final SoftwareInfo softwareInfo1 = SoftwareInfo.getInstance(sib1);

    final SoftwareInfoBean sib2 = new SoftwareInfoBean();
    sib2.setProgramName(programName2);
    sib2.setImplementationVersion(implementationVersion2);
    final SoftwareInfo softwareInfo2 = SoftwareInfo.getInstance(sib2);

    final SoftwareInfoPair sip = new SoftwareInfoPair();
    sip.setBurned(softwareInfo1);
    sip.setRegistered(softwareInfo2);

    return sip;
  }
}
