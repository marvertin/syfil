package cz.tconsult.lib.softwareinfo.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import cz.tconsult.lib.softwareinfo.SoftwareInfo;

/**
 */
public class SoftwareInfoTest {

  @Test
  public void testProgramName() throws Exception {
    final SoftwareInfo softwareInfo = SoftwareInfo.getInstance(SoftwareInfoTest.class);
    assertEquals(softwareInfo.getProgramName(), "SoftwareInfoTest");
  }
}
