pub trait VecExt<T> {
    fn prepend<I>(&mut self, others: I)
    where
        I: IntoIterator<Item = T>;
}

impl<T> VecExt<T> for Vec<T> {
    fn prepend<I>(&mut self, others: I)
    where
        I: IntoIterator<Item = T>,
    {
        let tmp = std::mem::replace(self, others.into_iter().collect());
        self.extend(tmp);
    }
}
