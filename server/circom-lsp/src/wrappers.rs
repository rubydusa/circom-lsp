use std::fmt;

// circom's program archive doesn't implement debug
pub struct ProgramArchive {
    pub inner: circom_structure::program_archive::ProgramArchive,
}

impl ProgramArchive {
    pub fn new(inner: circom_structure::program_archive::ProgramArchive) -> ProgramArchive {
        ProgramArchive { inner }
    }
}

impl fmt::Debug for ProgramArchive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ProgramArchive").finish()
    }
}
